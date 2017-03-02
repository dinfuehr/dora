#!/usr/bin/ruby

require 'pathname'
require 'tempfile'

$ARGS = ARGV.clone
$release = $ARGS.delete("--release") != nil
$no_capture = $ARGS.delete("--no-capture") != nil

$temp_out = Tempfile.new('runner_out')
$temp_err = Tempfile.new('runner_err')

class TestExpectation
  attr_accessor :fail,
                :position,
                :code,
                :message,
                :args,
                :output,
                :file

  def initialize(opts = {})
    fail = opts.fetch(:fail, false)

    self.fail = fail
  end
end

def test_files
  if $ARGS.length > 0
    $ARGS
  else
    Dir["tests/**/*.dora"]
  end
end

def run_tests
  tests = 0
  passed = 0
  failed = 0

  for file in test_files
    file = Pathname.new(file)
    tests += 1

    print "test #{file} ... "
    puts if $no_capture

    res = run_test(file)
    if res == true
      puts "ok"
      passed += 1

    elsif res == :ignore
      puts "ignore"

    else
      print "failed"
      print " (#{res})" if res != false
      puts

      failed += 1
    end
  end

  puts
  puts "#{passed} tests passed; #{failed} tests failed"

  failed == 0
end

def run_test(file)
  expectation = test_case_expectation(file)
  return :ignore if expectation == :ignore

  args = ""
  args = expectation.args.join(" ") if expectation.args

  target = $release ? "release" : "debug"
  testfile = expectation.file || file

  if $no_capture
    out_args = "2>&1 | tee #{$temp_out.path}"
  else
    out_args = ">#{$temp_out.path} 2>&1"
  end

  system("target/#{target}/dora #{testfile} #{args} #{out_args}")
  process = $?
  exit_code = process.exitstatus

  if expectation.fail
    return "expected failure (test exited with 0)" if exit_code == 0
    return "expected failure (#{expectation.code} expected but test returned #{exit_code})" if
      expectation.code && exit_code != expectation.code

    position, message = read_error_message($temp_out)
    return "position does not match (#{position.inspect} != #{expectation.position.inspect})" if
      expectation.position && position != expectation.position
    return "message does not match (#{message.inspect} != #{expectation.message.inspect})" if
      expectation.message && message != expectation.message

  elsif exit_code != 0
    return "expected success (0 expected but test returned #{exit_code})"

  end

  return "output does not match" if
    expectation.output && expectation.output != IO.read($temp_out.path)

  true
end

def read_error_message(file)
  position = nil
  message = nil

  for line in File.read(file).lines
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

        case arguments[1]
        when "at" then exp.position = arguments[2]
        when "code" then exp.code = arguments[2].to_i
        when "message" then exp.message = arguments[2].to_s
        when "assert" then exp.code = 101
        when "array" then exp.code = 102
        when "nil" then exp.code = 103
        when "exception" then exp.code = 104
        when "cast" then exp.code = 105
        when "unexpected" then exp.code = 106
        when "fail"
          # do nothing
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

      end
    end
  end

  exp
end

exit run_tests ? 0 : 1
