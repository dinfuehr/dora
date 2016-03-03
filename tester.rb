#!/usr/bin/ruby

require 'pathname'

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
  ec = expected_exit_code(file)

  system("target/debug/dora #{file} >/dev/null 2>&1")
  process = $?

  process.exitstatus == ec
end

def expected_exit_code(file)
  first_line = File.open(file, "r") do |f|
    f.readline
  end

  if (m = first_line.match(/^\/\/ error: (\d+)$/)) != nil
    m[1].to_i
  else
    0
  end
end

exit run_tests ? 0 : 1
