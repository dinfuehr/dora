#!/usr/bin/ruby

require 'pathname'
require 'tempfile'
require 'thread'
require 'open3'
require 'timeout'

def get_architecture
  case RUBY_PLATFORM
  when /linux/
  when /darwin/
    arch = `uname -m`.to_s.strip

    case arch
    when "x86_64" then "x64"
    when "arm64" then "arm64"
    else
      raise "unknown architecture #{arch}"
    end
  else
    # Windows
    arch = ENV["PROCESSOR_ARCHITECTURE"]
    case arch
    when "AMD64" then "x64"
    else
      raise "unknown architecture #{arch}"
    end
  end
end

def get_os
  case RUBY_PLATFORM
  when /linux/
    :linux
  when /darwin/
    :macos
  else
    if Gem.win_platform?
      :windows
    else
      raise "unknown operating system #{arch}"
    end
  end
end

def create_platform_binding
  arch = $ARCH
  os = $OS
  linux = $OS == :linux
  windows = $OS == :windows
  macos = $OS == :macos
  x64 = $ARCH == "x64"
  arm64 = $ARCH == "arm64"

  binding
end

$release = false
$capture = true
$stress = false
$stress_timeout = nil
$processors = nil
$forced_timeout = nil
$ARCH = get_architecture
$OS = get_os
$files = []
$platform_binding = create_platform_binding
$all_configs = {
  default: '',
  region: '--gc=region'
}

def process_arguments
  idx = 0
  while idx < ARGV.length do
    arg = ARGV[idx].to_s.strip

    if (m = /\A\-j(\d+)\z/.match(arg))
      $processors = m[1].to_i
      $processors = 1 if $processors < 1
    elsif (m = /\A\-\-timeout\=(\d+)\z/.match(arg))
      $forced_timeout = m[1].to_i
    elsif (m = /\A\-\-stress\=(\d+)\z/.match(arg))
      $stress = true
      $stress_timeout = m[1].to_i
      $stress_timeout = 60 if $stress_timeout < 1
    elsif (m = /\A\-\-binary\=(\S+)\z/.match(arg))
      $binary = m[1].to_s
    elsif arg == "--binary"
      $binary = ARGV[idx+1].to_s.strip
      idx += 1
    elsif arg == "--release"
      $release = true
    elsif arg == "--no-capture"
      $capture = false
    elsif arg == "--stress"
      $stress = true
      $stress_timeout = 60
    else
      $files.push(arg)
    end

    idx += 1
  end
end

def binary_path
  return $binary if $binary
  dir = $release ? "release" : "debug"
  extension = Gem.win_platform? ? ".exe" : ""
  "target/#{dir}/dora#{extension}"
end

class TestUtility
  def self.spawn_with_timeout(cmd, timeout)
    result = {
      :pid     => nil,
      :status  => nil,
      :stdout  => nil,
      :stderr  => nil,
      :timeout => false,
    }

    out_reader = nil
    err_reader = nil

    Open3.popen3(cmd) do | stdin, stdout, stderr, wait_thr |
      Timeout.timeout(timeout) do
        result[:pid] = wait_thr.pid

        stdin.close
        out_reader = Thread.new { stdout.read }
        err_reader = Thread.new { stderr.read }

        result[:status] = wait_thr.value
      end
    rescue Timeout::Error
      result[:timeout] = true

      Process.kill(:TERM, result[:pid])
    ensure
      result[:status] = wait_thr.value if wait_thr
      result[:stdout] = out_reader.value if out_reader
      result[:stderr] = err_reader.value if err_reader
      stdout.close unless stdout.closed?
      stderr.close unless stderr.closed?
    end

    result
  end
end

class TestExpectation
  attr_accessor :fail,
                :position,
                :code,
                :message,
                :stdout,
                :stderr

  def initialize(opts = {})
    fail = opts.fetch(:fail, false)

    self.fail = fail
  end
end

class TestCase
  attr_accessor :file,
                :test_file,
                :vm_args,
                :args,
                :expectation,
                :result,
                :timeout,
                :configs

  def initialize(file, opts = {})
    self.expectation = opts.fetch(:expectation, TestExpectation.new(fail: false))
    self.file = self.test_file = file
    self.args = self.vm_args = ""
    self.configs = [:default]
  end

  def get_timeout
    if $forced_timeout
      $forced_timeout

    elsif self.timeout
      self.timeout

    else
      60

    end
  end
end

TestResult = Struct.new(:test_case, :config, :status, :message)

def num_from_shell(cmd)
  begin
    stdout, stderr, status = Open3.capture3(cmd)

    if status.success?
      stdout.to_i
    else
      0
    end
  rescue
    0
  end
end

def query_number_processors
  case RUBY_PLATFORM
  when /linux/
    num = num_from_shell("nproc --all")
    return num if num > 0

    num = num_from_shell("grep -c ^processor /proc/cpuinfo")
    return num if num > 0

  when /darwin/
    num = num_from_shell("sysctl -n hw.ncpu")
    return num if num > 0

  else
    # Windows
    num = ENV["NUMBER_OF_PROCESSORS"].to_i
    return num if num > 0

  end

  1
end

def load_test_files
  if $files.length > 0
    files = []

    for arg in $files
      if File.directory?(arg)
        files.concat(Dir["#{arg}/**/*.dora"])
      elsif File.file?(arg)
        files.push(arg)
      else
        puts "#{arg} is not a file or directory."
        exit 1
      end
    end

    files

  else
    Dir["tests/**/*.dora"]
  end
end

def run_tests
  tests = 0
  passed = 0
  failed = 0
  ignore = 0

  mutex = Mutex.new
  threads = []
  faillist = []
  cancel = false

  # Load all test files and shuffle them around to run tests
  # in different order
  test_files = load_test_files
  worklist = parse_test_files(test_files).shuffle

  if $stress && worklist.empty?
    puts "--stress needs at least one test."
    exit 1
  end

  unless File.file?(binary_path)
    puts "no executable #{binary_path} found"
    exit 1
  end

  computed_processors = query_number_processors

  number_threads =
    if $processors
      $processors
    elsif $stress
      computed_processors * 2
    else
      computed_processors
    end

  number_threads.times do
    thread = Thread.new do
      loop do
        test_with_config = mutex.synchronize do
          if $stress
            test_idx = rand(worklist.size)
            cancel ? nil : worklist[test_idx]
          else
            worklist.pop
          end
        end

        break unless test_with_config
        test_case, config = test_with_config

        test_result = run_test(test_case, config, mutex)

        case test_result.status
        when :ignore then ignore += 1
        when :passed then passed += 1
        when :failed then failed += 1
        else
          raise "unknown status #{test_result.status.inspect}"
        end

        mutex.synchronize do
          faillist.push(test_with_config) if test_result.status == :failed
          print_result(test_case, config, test_result)
          STDOUT.flush

        end
      end
    end

    threads.push(thread)
  end

  if $stress
    sleep $stress_timeout
    mutex.synchronize do
      cancel = true
    end
  end

  for thread in threads do
    thread.join
  end

  ret_success = failed == 0

  if faillist.any?
    puts "failed tests:"

    for test_failure in faillist
      test_case, config = test_failure
      puts "    #{test_case.file}.#{config}"
    end
  end

  passed = "#{passed} #{test_name(passed)} passed"
  failed = "#{failed} #{test_name(failed)} failed"

  if ignore > 0
    ignore = "#{ignore} #{test_name(ignore)} ignored"
    puts "#{passed}; #{ignore}; #{failed}"
  else
    puts "#{passed}; #{failed}"
  end

  ret_success
end

def run_test(test_case, config, mutex)
  if test_case.expectation == :ignore
    return TestResult.new(test_case, config, :ignore, nil)
  end

  cmdline = "#{binary_path} #{$all_configs[config]} #{test_case.vm_args} #{test_case.test_file} #{test_case.args}"
  process_result = TestUtility.spawn_with_timeout(cmdline, test_case.get_timeout)
  result = check_test_run_result(test_case, process_result)

  if !$capture || result != true
    mutex.synchronize do
      puts "#==== STDOUT"
      puts process_result[:stdout] unless process_result[:stdout].empty?
      puts "#==== STDERR"
      puts process_result[:stderr] unless process_result[:stderr].empty?
      puts "RUN: #{cmdline}"
      STDOUT.flush
    end
  end

  if result == true
    TestResult.new(test_case, config, :passed, nil)
  else
    TestResult.new(test_case, config, :failed, result)
  end
end

def check_test_run_result(test_case, result)
  status = result[:status]
  stdout = result[:stdout]
  stderr = result[:stderr]
  timeout = result[:timeout]
  exit_code = status.exitstatus

  return "test timed out after #{test_case.get_timeout} seconds" if
    timeout

  if test_case.expectation.fail
    position, message = read_error_message(stderr)

    return "expected failure (test exited with 0)" if exit_code == 0
    return "expected failure (#{test_case.expectation.code} expected but test returned #{status})" if
      test_case.expectation.code && exit_code != test_case.expectation.code

    return "position does not match (#{position.inspect} != #{test_case.expectation.position.inspect})" if
      test_case.expectation.position && position != test_case.expectation.position
    return "message does not match (#{message.inspect} != #{test_case.expectation.message.inspect})" if
      test_case.expectation.message && message != test_case.expectation.message

  elsif exit_code != 0
    return "expected success (0 expected but test returned #{status})"

  end

  return "stdout does not match (expected #{test_case.expectation.stdout.inspect} but got #{stdout.inspect})" if
    test_case.expectation.stdout && test_case.expectation.stdout != stdout

  return "stderr does not match (expected #{test_case.expectation.stderr.inspect} but got #{stderr.inspect})" if
    test_case.expectation.stderr && test_case.expectation.stderr != stderr

  true
end

def print_result(test_case, config, test_result)
  if test_result.status == :ignore
    puts "#{test_case.file} ... ignore"
    return
  end

  print "#{test_case.file}.#{config}... "

  if test_result.status == :passed
    print "ok"
  elsif test_result.status == :failed
    print "failed"
    print " (#{test_result.message})" if test_result.message
  else
    raise "unknown status #{test_result.status.inspect}"
  end
  puts
end

def test_name(num)
  num == 1 ? "test" : "tests"
end

def read_error_message(content)
  position = nil
  message = nil

  content.each_line do |line|
    line = line.strip

    if line == "1 error found." || line == "error during parsing."
      return position, message

    elsif (m = line.match(/^error in (.+) at (\d+:\d+): (.+)$/)) != nil
      position = m[2].to_s
      message = m[3].to_s

    elsif (m = line.match(/^error at (\d+:\d+): (.+)$/)) != nil
      position = m[1].to_s
      message = m[2].to_s
    end
  end

  return nil, nil
end

def read_cmdline(text)
  args = []
  in_quote = false
  escaped = false
  arg = ""

  for char in text.chars
    if escaped
      arg +=  case char
              when "n" then "\n"
              when "t" then "\t"
              else
                return "unknown escape sequence \\#{char}"
              end

      escaped = false
      next
    end

    case char
    when " "
      if in_quote
        arg += " "
      else
        args.push(arg) unless arg.empty?
        arg = ""
      end

    when "\""
      if in_quote
        args.push(arg)
        arg = ""

      elsif arg.empty?
        in_quote = true

      else
        arg += "\""

      end
    when "\\"
      if in_quote
        escaped = true
      else
        arg += "\\"
      end

    else
      arg += char

    end
  end

  args.push(arg) unless arg.empty?

  args
end

def parse_test_files(files)
  tests = []

  for file in files
    test_cases = parse_test_file(file)
    tests += test_cases
  end

  tests
end

def parse_test_file(file)
  test_case = TestCase.new(file)

  for line in File.read(file).lines
    line = line.strip

    if line.start_with?("//=")
      line = line[3..-1].strip

      arguments = read_cmdline(line)

      case arguments[0]
      when "error"
        test_case.expectation.fail = true

        next if arguments.size == 1

        case arguments[1]
        when "at" then test_case.expectation.position = arguments[2]
        when "code" then test_case.expectation.code = arguments[2].to_i
        when "message" then test_case.expectation.message = arguments[2].to_s
        when "div0" then test_case.expectation.code = 101
        when "assert" then test_case.expectation.code = 102
        when "array" then test_case.expectation.code = 103
        when "nil" then test_case.expectation.code = 104
        when "cast" then test_case.expectation.code = 105
        when "oom" then test_case.expectation.code = 106
        when "stack-overflow" then test_case.expectation.code = 107
        when "fail"
          # do nothing
        else
          raise "unknown error expectation in #{file}: #{line}"
        end

      when "platform"
        supported = $platform_binding.eval(arguments[1])
        test_case.expectation = :ignore unless supported

      when "file"
        test_case.test_file = arguments[1]

      when "ignore" 
        test_case.expectation = :ignore

      when "stdout"
        case arguments[1]
        when "file" then test_case.expectation.stdout = IO.read(file.sub(".dora", ".stdout"))
        else
          test_case.expectation.stdout = arguments[1]
        end

      when "stderr"
        test_case.expectation.stderr = arguments[1]

      when "config"
        config = arguments[1].intern
        raise "unknown config #{arguments[1]}" unless $all_configs.include?(config)
        test_case.configs << config

      when "args"
        test_case.args = arguments[1..-1].join(" ")

      when "vm-args"
        test_case.vm_args = arguments[1..-1].join(" ")

      when "boots"
        test_case.vm_args += '--boots=dora-boots --gc-verify'

      when "timeout"
        test_case.timeout = arguments[1].to_i

      else
        raise "unkown expectation in #{file}: #{line}"

      end
    end
  end

  tests = test_case.configs.map do |config|
    [test_case, config]
  end

  tests
end

process_arguments
exit run_tests ? 0 : 1
