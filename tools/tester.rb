#!/usr/bin/ruby

require 'pathname'
require 'tempfile'
require 'thread'
require 'open3'
require 'timeout'

$config = {
  cannon: '--compiler=cannon',
}

$ARGS = ARGV.clone
$release = $ARGS.delete("--release") != nil
$no_capture = $ARGS.delete("--no-capture") != nil
$processors = 0

$ARGS.delete_if do |arg|
  if (m = /\A\-j(\d)+\z/.match(arg))
    $processors = m[1].to_i
    true
  elsif (m = /\A\-\-binary\=(\S+)\z/.match(arg))
    $binary = m[1].to_s
    true
  else
    false
  end
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
                :configs,
                :results,
                :timeout

  def initialize(file, opts = {})
    self.expectation = opts.fetch(:expectation, TestExpectation.new(fail: false))
    self.file = self.test_file = file
    self.configs = [:cannon]
    self.results = {}
    self.args = self.vm_args = ""
    self.timeout = 60
  end

  def run(mutex)
    if self.expectation == :ignore
      self.results = :ignore 
      return {:ignore => 1}
    end
    configs.each do |optional_config|
      self.results[optional_config] = run_test($config[optional_config], mutex)
    end

    if self.results.empty? 
      self.results = :ignore
      return { :ignore => 1 } 
    end

    return {
      :passed => self.results.values().count(true), 
      :failed => self.results.values().count{|x| x != true} 
    }
  end

  def print_results() 
    if self.results == :ignore
      puts "#{self.file} ... ignore"
      return
    end

    self.results.each_pair do |run_name, run_result|
      print "#{self.file}.#{run_name}"
      print "... "

      if run_result == true
        print "ok"
      else
        print "failed"
        print " (#{run_result})" if run_result != false 
      end
      puts
    end
  end

  private
  def run_test(optional_vm_args, mutex)
    cmdline = "#{binary} #{vm_args} #{optional_vm_args} #{test_file} #{args}"
    process_result = TestUtility.spawn_with_timeout(cmdline, self.timeout)
    result = check_test_run_result(process_result)
    if $no_capture || result != true
      mutex.synchronize do
        puts "#==== STDOUT"
        puts process_result[:stdout] unless process_result[:stdout].empty?
        puts "#==== STDERR"
        puts process_result[:stderr] unless process_result[:stderr].empty?
        puts "RUN: #{cmdline}"
        STDOUT.flush
      end
    end
    result
  end

  def binary
    return $binary if $binary
    dir = $release ? "release" : "debug"
    "target/#{dir}/dora"
  end

  def check_test_run_result(result)
    status = result[:status]
    stdout = result[:stdout]
    stderr = result[:stderr]
    timeout = result[:timeout]
    exit_code = status.exitstatus

    return "test timed out after #{self.timeout} seconds" if
      timeout

    if self.expectation.fail
      position, message = read_error_message(stderr)

      return "expected failure (test exited with 0)" if exit_code == 0
      return "expected failure (#{self.expectation.code} expected but test returned #{status})" if
        self.expectation.code && exit_code != self.expectation.code
  
      return "position does not match (#{position.inspect} != #{self.expectation.position.inspect})" if
        self.expectation.position && position != expectation.position
      return "message does not match (#{message.inspect} != #{self.expectation.message.inspect})" if
        self.expectation.message && message != self.expectation.message
  
    elsif exit_code != 0
      return "expected success (0 expected but test returned #{status})"
  
    end
  
    return "stdout does not match (expected #{self.expectation.stdout.inspect} but got #{stdout.inspect})" if
      self.expectation.stdout && self.expectation.stdout != stdout

    return "stderr does not match (expected #{self.expectation.stderr.inspect} but got #{stderr.inspect})" if
      self.expectation.stderr && self.expectation.stderr != stderr

    true
  end
end

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

def number_processors
  return $processors if $processors > 0

  case RUBY_PLATFORM
  when /linux/
    num = num_from_shell("nproc --all")
    return num if num > 0

    num = num_from_shell("grep -c ^processor /proc/cpuinfo")
    return num if num > 0

  when /darwin/
    num = num_from_shell("sysctl -n hw.ncpu")
    return num if num > 0
  end

  1
end

def test_files
  if $ARGS.length > 0
    files = []

    for arg in $ARGS
      if File.directory?(arg)
        files.concat(Dir["#{arg}/**/*.dora"])
      else
        files.push(arg)
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

  # Load all test files and shuffle them around to run tests
  # in different order
  worklist = test_files.shuffle

  number_processors.times do
    thread = Thread.new do
      loop do
        file = mutex.synchronize { worklist.pop }
        break unless file

        test_case = parse_test_file(Pathname.new(file))
        test_results = test_case.run(mutex)

        test_results.each_pair do |key, value|
          passed += value if key == :passed
          ignore += value if key == :ignore
          failed += value if key == :failed
        end

        mutex.synchronize do
          faillist.push(test_case) if test_results.any? { |key,value| key == :failed && value > 0 }
          test_case.print_results
          STDOUT.flush

        end
      end
    end

    threads.push(thread)
  end

  for thread in threads do
    thread.join
  end

  ret = failed == 0


  if faillist.any?
    puts "failed tests:"

    for test_case in faillist
      for run_name, run_result in test_case.results
        next if run_result == true
        puts "    #{test_case.file}.#{run_name}"
      end
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

  ret
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

      when "file"
        test_case.test_file = arguments[1]

      when "ignore" 
        test_case.expectation = :ignore
        return test_case

      when "stdout"
        case arguments[1]
        when "file" then test_case.expectation.stdout = IO.read(file.sub(".dora", ".stdout"))
        else
          test_case.expectation.stdout = arguments[1]
        end

      when "stderr"
        test_case.expectation.stderr = arguments[1]

      when "args"
        test_case.args = arguments[1..-1].join(" ")

      when "vm-args"
        test_case.vm_args = arguments[1..-1].join(" ")

      when "cannon-only"
        test_case.configs = [:cannon]

      when "boots"
        test_case.args += '--boots=dora-boots --gc-verify'

      when "timeout"
        test_case.timeout = arguments[1].to_i

      else
        raise "unkown expectation in #{file}: #{line}"

      end
    end
  end

  test_case
end

exit run_tests ? 0 : 1
