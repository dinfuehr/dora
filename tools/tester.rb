#!/usr/bin/ruby

require 'pathname'
require 'tempfile'
require 'thread'
require 'open3'

$ARGS = ARGV.clone
$release = $ARGS.delete("--release") != nil
$no_capture = $ARGS.delete("--no-capture") != nil
$processors = 0
$filter_run = nil

$ARGS.delete_if do |arg|
  if (m = /\A\-j(\d)+\z/.match(arg))
    $processors = m[1].to_i
    true
  else
    false
  end
end

class TestExpectation
  attr_accessor :fail,
                :position,
                :code,
                :message,
                :output

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
                :target,
                :expectation,
                :optional_runs,
                :results

  def initialize(file, opts = {})
    self.expectation = opts.fetch(:expectation, TestExpectation.new(fail: false))
    self.file = self.test_file = file
    self.optional_runs = {:main => ''}
    self.results = {}
    self.args = self.vm_args = ""
    self.target = $release ? "release" : "debug"
  end

  def run()
    if self.expectation == :ignore
      self.results = :ignore 
      return {:ignore => 1}
    end
    optional_runs.each_pair do |optional_run, run_vm_args|   
      if $filter_run != nil
        if $filter_run == 'standard'
          if optional_run != nil
            next
          end
        elsif $filter_run != optional_run
          next
        end
      end
      self.results[optional_run] = run_test(run_vm_args) 
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
      print "[#{run_name}] " if run_name != :main
      print "#{self.file} "
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
  def run_test(optional_vm_args)
    temp_out = Tempfile.new("dora-test-runner")
    out_args = ">#{temp_out.path} 2>&1"

    system("target/#{target}/dora #{vm_args} #{optional_vm_args} #{test_file} #{args} #{out_args}")

    process = $?
    exit_code = process.exitstatus
  
    temp_out_content = IO.read(temp_out.path)
    temp_out.close

    position, message = read_error_message(temp_out_content)

    return check_test_run_result(exit_code, position, message, temp_out_content)
  end

  def check_test_run_result(exit_code, position, message, content)
    if self.expectation.fail
      return "expected failure (test exited with 0)" if exit_code == 0
      return "expected failure (#{self.expectation.code} expected but test returned #{exit_code})" if 
        self.expectation.code && exit_code != self.expectation.code
  
      return "position does not match (#{position.inspect} != #{self.expectation.position.inspect})" if
        self.expectation.position && position != expectation.position
      return "message does not match (#{message.inspect} != #{self.expectation.message.inspect})" if
        self.expectation.message && message != self.expectation.message
  
    elsif exit_code != 0
      return "expected success (0 expected but test returned #{exit_code})"
  
    end
  
    return "output does not match (#{self.expectation.output.inspect} != #{content.inspect})" if
      self.expectation.output && self.expectation.output != content
  
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

  num = num_from_shell("nproc --all")
  return num if num > 0

  num = num_from_shell("grep -c ^processor /proc/cpuinfo")
  return num if num > 0

  num = num_from_shell("sysctl -n hw.ncpu")
  return num if num > 0

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
  worklist = test_files.dup

  number_processors.times do
    thread = Thread.new do
      loop do
        file = mutex.synchronize { worklist.pop }
        break unless file

        test_case = parse_test_file(Pathname.new(file))
        test_results = test_case.run()

        test_results.each_pair do |key, value|
          passed += value if key == :passed
          ignore += value if key == :ignore
          failed += value if key == :failed
        end

        mutex.synchronize do
          test_case.print_results

        end
      end
    end

    threads.push(thread)
  end

  for thread in threads do
    thread.join
  end

  ret = failed == 0

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

    if line == "1 error found."
      return position, message

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
        when "exception" then test_case.expectation.code = 105
        when "cast" then test_case.expectation.code = 106
        when "unexpected" then test_case.expectation.code = 107
        when "oom" then test_case.expectation.code = 108
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

      when "output"
        case arguments[1]
        when "file" then test_case.expectation.output = IO.read(file.sub(".dora", ".result"))
        else
          test_case.expectation.output = arguments[1]
        end

      when "args"
        test_case.args = arguments[1..-1].join(" ")

      when "vm-args"
        test_case.vm_args = arguments[1..-1].join(" ")

      when "cannon"
        test_case.optional_runs['cannon'] = '--bc=cannon'

      else
        raise "unkown expectation in #{file}: #{line}"

      end
    end
  end

  test_case
end

exit run_tests ? 0 : 1
