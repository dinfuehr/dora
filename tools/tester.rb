#!/usr/bin/ruby

require 'pathname'
require 'tempfile'
require 'thread'
require 'open3'

$ARGS = ARGV.clone
$release = $ARGS.delete("--release") != nil
$no_capture = $ARGS.delete("--no-capture") != nil
$processors = 0

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
                :args,
                :vm_args,
                :output,
                :file

  def initialize(opts = {})
    fail = opts.fetch(:fail, false)

    self.fail = fail
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

        file = Pathname.new(file)
        res = run_test(file)

        mutex.synchronize do
          print "#{file} ... "

          if res == true
            puts "ok"
            passed += 1

          elsif res == :ignore
            puts "ignore"
            ignore += 1

          else
            print "failed"
            print " (#{res})" if res != false
            puts

            failed += 1
          end
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

def run_test(file)
  expectation = test_case_expectation(file)
  return :ignore if expectation == :ignore

  temp_out = Tempfile.new('dora-test-runner')

  args = ""
  args = expectation.args.join(" ") if expectation.args

  vm_args = ""
  vm_args = expectation.vm_args.join(" ") if expectation.vm_args

  target = $release ? "release" : "debug"
  testfile = expectation.file || file

  out_args = ">#{temp_out.path} 2>&1"

  system("target/#{target}/dora #{vm_args} #{testfile} #{args} #{out_args}")
  process = $?
  exit_code = process.exitstatus

  temp_out_content = IO.read(temp_out.path)
  temp_out.close

  if expectation.fail
    return "expected failure (test exited with 0)" if exit_code == 0
    return "expected failure (#{expectation.code} expected but test returned #{exit_code})" if
      expectation.code && exit_code != expectation.code

    position, message = read_error_message(temp_out_content)
    return "position does not match (#{position.inspect} != #{expectation.position.inspect})" if
      expectation.position && position != expectation.position
    return "message does not match (#{message.inspect} != #{expectation.message.inspect})" if
      expectation.message && message != expectation.message

  elsif exit_code != 0
    return "expected success (0 expected but test returned #{exit_code})"

  end

  return "output does not match" if
    expectation.output && expectation.output != temp_out_content

  true
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

def test_case_expectation(file)
  exp = TestExpectation.new(fail: false)

  for line in File.read(file).lines
    line = line.strip

    if line.start_with?("//=")
      line = line[3..-1].strip

      arguments = read_cmdline(line)

      case arguments[0]
      when "error"
        exp.fail = true

        next if arguments.size == 1

        case arguments[1]
        when "at" then exp.position = arguments[2]
        when "code" then exp.code = arguments[2].to_i
        when "message" then exp.message = arguments[2].to_s
        when "div0" then exp.code = 101
        when "assert" then exp.code = 102
        when "array" then exp.code = 103
        when "nil" then exp.code = 104
        when "exception" then exp.code = 105
        when "cast" then exp.code = 106
        when "unexpected" then exp.code = 107
        when "oom" then exp.code = 108
        when "fail"
          # do nothing
        else
          raise "unknown error expectation in #{file}: #{line}"
        end

      when "file"
        exp.file = arguments[1]

      when "ignore" then return :ignore

      when "output"
        case arguments[1]
        when "file" then exp.output = IO.read(file.sub(".dora", ".result"))
        else
          exp.output = arguments[1]
        end

      when "args"
        exp.args = arguments[1..-1]

      when "vm-args"
        exp.vm_args = arguments[1..-1]

      else
        raise "unkown expectation in #{file}: #{line}"

      end
    end
  end

  exp
end

exit run_tests ? 0 : 1
