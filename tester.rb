#!/usr/bin/ruby

require 'pathname'
require 'tempfile'

$temp_out = Tempfile.new('runner_out')
$temp_err = Tempfile.new('runner_err')

class TestExpectation
  attr_accessor :fail,
                :position,
                :code,
                :message,
                :args

  def initialize(opts = {})
    fail = opts.fetch(:fail, false)

    self.fail = fail
  end
end

def run_tests
  tests = 0
  passed = 0
  failed = 0

  for file in Dir["tests/**/*.dora"]
    file = Pathname.new(file)
    tests += 1

    print "test #{file} ... "

    if run_test(file)
      puts "ok"
      passed += 1
    else
      puts "failed"
      failed += 1
    end
  end

  puts
  puts "#{passed} tests passed; #{failed} tests failed"

  tests == passed
end

def run_test(file)
  expectation = test_case_expectation(file)

  args = ""
  args = expectation.args.join(" ") if expectation.args

  system("target/debug/dora #{file} #{args} >#{$temp_out.path} 2>/dev/null")
  process = $?
  exit_code = process.exitstatus

  if expectation.fail
    return false if exit_code == 0
    return false if expectation.code && exit_code != expectation.code

    position, message = read_error_message($temp_out)
    return false if
      position && expectation.position && position != expectation.position
    return false if
      message && expectation.message && message != expectation.message

    true
  else
    exit_code == 0

  end
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

def test_case_expectation(file)
  exp = TestExpectation.new(fail: false)

  for line in File.read(file).lines
    line = line.strip

    if line.start_with?("//=")
      line = line[3..-1].strip

      arguments = line.split(/\s+/)

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
        end

      when "args"
        exp.args = arguments[1..-1]

      end
    end
  end

  exp
end

exit run_tests ? 0 : 1
