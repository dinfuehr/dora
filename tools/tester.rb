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

class Config
  attr_accessor :name
  attr_accessor :flags
  attr_accessor :directories
  attr_accessor :enable_boots

  def initialize(name, flags, directories)
    self.name = name
    self.flags = flags
    self.directories = directories
    self.enable_boots = false
  end

  def enabled_for?(test_dir)
    return true if @directories == true
    for dir in @directories
      if test_dir == $tests_dir.join(dir)
        return true
      end
    end
    false
  end
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

$default_config = Config.new("default", "", true)

$always_boots_config = Config.new("always_boots", '--always-boots', true)
$always_boots_config.enable_boots = true

$all_configs = [
  $default_config,
  $always_boots_config,
]
$force_config = nil
$select_config = nil
$config = nil
$exit_after_n_failures = nil
$env = {}
$verbose = false
$check_only = false
$extra_args = nil
dir = File.expand_path(__dir__)
$tests_dir = Pathname.new(dir).parent.join('tests')

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
    elsif (m = /\A\-\-exit\-after\-n\-failures\=(\d+)\z/.match(arg))
      $exit_after_n_failures = m[1].to_i
    elsif arg == "--target"
      $target = ARGV[idx+1].to_s.strip
      idx += 1
    elsif arg == "--env"
      raise "missing arguments" unless idx+1 < ARGV.length
      name_and_value = ARGV[idx+1].to_s.split("=", 2)
      raise "missing value" unless name_and_value.length == 2
      $env[name_and_value[0]] = name_and_value[1]
      idx += 1
    elsif arg == "--force-config"
      config_name = ARGV[idx+1].to_s.strip
      $force_config = $all_configs.detect { |c| c.name == config_name }
      raise "unknown config #{config_name}" unless $force_config
      idx += 1
    elsif arg == "--select-config"
      config_name = ARGV[idx+1].to_s.strip
      $select_config = $all_configs.detect { |c| c.name == config_name }
      raise "unknown config #{config_name}" unless $select_config
      idx += 1
    elsif arg == "--release"
      $release = true
    elsif arg == "--extra-args"
      $extra_args = ARGV[idx+1].to_s.strip
      idx += 1
    elsif arg == "--no-capture"
      $capture = false
    elsif arg == "--stress"
      $stress = true
      $stress_timeout = 60
    elsif arg == "--verbose"
      $verbose = true
    elsif arg == "--check"
      $check_only = true
    else
      $files.push(arg)
    end

    idx += 1
  end
end

def binary_path
  dir = $release ? "release" : "debug"
  extension = Gem.win_platform? ? ".exe" : ""
  target = $target ? "target/#{$target}" : "target"
  "#{target}/#{dir}/dora#{extension}"
end

class ProcessResult
  attr_accessor :pid
  attr_accessor :status
  attr_accessor :process_status
  attr_accessor :stdout
  attr_accessor :stderr
  attr_accessor :timeout

  def initialize
    self.stdout = ""
    self.stderr = ""
  end

  def compute_status(process_status)
    self.process_status = process_status

    if process_status.exited?
      self.status = process_status.exitstatus
    elsif process_status.signaled?
      signal_code = process_status.termsig
      signal_name = Signal.signame(signal_code)
      signal_name = "unknown" unless signal_name
      self.status = "signal #{signal_name}/#{signal_code}"
    else
      raise "unknown process status #{process_status.inspect}"
    end
  end
end

def spawn_with_timeout(env, cmd, timeout)
  result = ProcessResult.new
  result.stdout = ""
  result.stderr = ""

  out_reader = nil
  err_reader = nil

  Open3.popen3(env, cmd) do | stdin, stdout, stderr, wait_thr |
    Timeout.timeout(timeout) do
      result.pid = wait_thr.pid

      stdin.close
      out_reader = Thread.new { stdout.read }
      err_reader = Thread.new { stderr.read }

      process_status = wait_thr.value
      result.compute_status(process_status)
    end
  rescue Timeout::Error
    result.timeout = true
    result.status = "TIMEOUT"

    Process.kill(:TERM, result.pid)
  ensure
    result.stdout = out_reader.value if out_reader
    result.stderr = err_reader.value if err_reader
    stdout.close unless stdout.closed?
    stderr.close unless stderr.closed?
  end

  result
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
                :configs,
                :enable_boots

  def initialize(file, opts = {})
    self.expectation = opts.fetch(:expectation, TestExpectation.new(fail: false))
    self.file = self.test_file = file
    self.args = self.vm_args = ""
    self.configs = []
    self.enable_boots = false
    @ignore = false
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

  def set_ignore
    @ignore = true
  end

  def ignore?
    @ignore
  end
end

class TestResult
  attr_accessor :test_case
  attr_accessor :config
  attr_accessor :status
  attr_accessor :message

  def self.error(test_case, config, message)
    result = TestResult.new
    result.test_case = test_case
    result.config = config
    result.status = :failed
    result.message = message
    result
  end

  def self.success(test_case, config)
    result = TestResult.new
    result.test_case = test_case
    result.config = config
    result.status = :passed
    result
  end

  def self.ignore(test_case, config)
    result = TestResult.new
    result.test_case = test_case
    result.config = config
    result.status = :ignore
    result
  end

  def success?
    self.status == :passed
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

def next_test(worklist, synchronization)
  return nil if synchronization.cancelled?

  if $stress
    test_idx = rand(worklist.size)
    worklist[test_idx]
  else
    worklist.pop
  end
end

class ThreadSynchronization
  attr_accessor :mutex
  attr_accessor :cv_cancel
  attr_accessor :cancelled

  def initialize
    self.mutex = Mutex.new
    self.cv_cancel = ConditionVariable.new
    self.cancelled = false
  end

  def cancel
    self.cancelled = true
    self.cv_cancel.broadcast
  end

  def cancelled?
    self.cancelled
  end

  def sleep_until_timeout_or_cancelled(timeout)
    self.mutex.synchronize do
      # wait `timeout` seconds for cancel signal
      self.cv_cancel.wait(mutex, timeout)

      # Either we reached timeout or one thread woke us up.
      # In any case make sure that all other threads stop as well.
      self.cancel
    end
  end
end

def run_tests
  tests = 0
  passed = 0
  failed = 0
  ignore = 0

  threads = []

  synchronization = ThreadSynchronization.new
  mutex = synchronization.mutex

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
          next_test(worklist, synchronization)
        end

        break unless test_with_config
        test_case, config = test_with_config

        test_result = run_test(test_case, config, mutex)

        mutex.synchronize do
          case test_result.status
          when :ignore then ignore += 1
          when :passed then passed += 1
          when :failed then failed += 1
          else
            raise "unknown status #{test_result.status.inspect}"
          end

          if test_result.status == :failed
            faillist.push(test_with_config)

            if $exit_after_n_failures && faillist.length >= $exit_after_n_failures
              synchronization.cancel
            end
          end
          print_result(test_case, config, test_result)
          STDOUT.flush

        end
      end
    end

    threads.push(thread)
  end

  if $stress
    synchronization.sleep_until_timeout_or_cancelled($stress_timeout)
  end

  for thread in threads do
    thread.join
  end

  ret_success = failed == 0

  if faillist.any?
    puts "failed tests:"

    for test_failure in faillist
      test_case, config = test_failure
      puts "    #{test_case.file}.#{config.name}"
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
  if test_case.ignore?
    return TestResult.ignore(test_case, config)
  end

  args = ""
  args << " #{config.flags}" unless config.flags.empty?
  args << " --package boots dora-boots/boots.dora" if test_case.enable_boots || config.enable_boots
  args << " --check" if $check_only
  args << " #{test_case.vm_args}" unless test_case.vm_args.empty?
  args << " #{$extra_args}" if $extra_args
  args << " #{test_case.test_file}"
  args << " #{test_case.args}" unless test_case.args.empty?

  cmdline = "#{binary_path}#{args}"
  puts cmdline if $verbose

  process_result = spawn_with_timeout($env, cmdline, test_case.get_timeout)
  result = check_process_result(test_case, process_result)

  if result == true
    result = TestResult.success(test_case, config)
  else
    raise "unexpected return value #{result.inspect}" unless String === result
    result = TestResult.error(test_case, config, result)
  end

  if !$capture || !result.success?
    mutex.synchronize do
      puts "#==== STDOUT"
      puts process_result.stdout unless process_result.stdout.empty?
      if test_case.expectation.stdout
        puts "#==== EXPECTED STDOUT"
        puts process_result.stdout
      end
      puts "#==== STDERR"
      puts process_result.stderr unless process_result.stderr.empty?
      if test_case.expectation.stderr
        puts "#==== EXPECTED STDERR"
        puts process_result.stderr
      end
      puts "RUN: #{cmdline}"
      puts "RUN: cargo run -p dora --#{args}"
      STDOUT.flush
    end
  end

  result
end

def check_process_result(test_case, result)
  return "test timed out after #{test_case.get_timeout} seconds" if
    result.timeout

  if $check_only
    return "semantic check failed" if result.status != 0
    return true
  end

  if test_case.expectation.fail
    position, message = read_error_message(result.stderr)

    return "expected failure (test exited with 0)" if result.status == 0
    return "expected failure (#{test_case.expectation.code} expected but test returned #{result.status})" if
      test_case.expectation.code && result.status != test_case.expectation.code

    return "position does not match (#{position.inspect} != #{test_case.expectation.position.inspect})" if
      test_case.expectation.position && position != test_case.expectation.position
    return "message does not match (#{message.inspect} != #{test_case.expectation.message.inspect})" if
      test_case.expectation.message && message != test_case.expectation.message

  elsif result.status != 0
    return "expected success (0 expected but test returned #{result.status})"

  end

  return "stdout does not match" if test_case.expectation.stdout && test_case.expectation.stdout != result.stdout
  return "stderr does not match" if test_case.expectation.stderr && test_case.expectation.stderr != result.stderr

  true
end

def print_result(test_case, config, test_result)
  if test_result.status == :ignore
    puts "#{test_case.file} ... ignore"
    return
  end

  print "#{test_case.file}.#{config.name}... "

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
  test_dir = Pathname.new(File.expand_path(file)).parent

  if $force_config
    test_case.configs.push($force_config)
  else
    for config in $all_configs do
      test_case.configs.push(config) if config.enabled_for?(test_dir)
    end

    if $select_config
      if test_case.configs.include?($select_config)
        test_case.configs = [$select_config]
      else
        test_case.set_ignore
      end
    end
  end

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
        when "code" then test_case.expectation.code = arguments[2].to_i
        when "div0" then test_case.expectation.code = 101
        when "assert" then test_case.expectation.code = 102
        when "array" then test_case.expectation.code = 103
        when "nil" then test_case.expectation.code = 104
        when "cast" then test_case.expectation.code = 105
        when "oom" then test_case.expectation.code = 106
        when "stack-overflow" then test_case.expectation.code = 107
        when "overflow" then test_case.expectation.code = 109
        else
          raise "unknown error expectation in #{file}: #{line}"
        end

      when "platform"
        supported = $platform_binding.eval(arguments[1])
        test_case.set_ignore unless supported

      when "file"
        test_case.test_file = arguments[1]

      when "ignore" 
        test_case.set_ignore

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
        test_case.vm_args += " " unless test_case.vm_args.empty?
        test_case.vm_args += arguments[1..-1].join(" ")

      when "boots"
        test_case.enable_boots = true

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
