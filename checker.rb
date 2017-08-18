#!/usr/bin/ruby

errors = 0
tests = 0
ignored = 0

def ignore(file)
  File.open(file).each_line do |line|
    return true if line.start_with?("//= error")
    return true if line.start_with?("//= file")
  end

  false
end

for file in Dir["tests/**/*.dora"]
  if ignore(file)
    puts "ignore #{file}"
    ignored += 1
    next
  end

  if !system("target/debug/dora", "--check", file)
    puts "#{file} failed"
    errors += 1
  end

  tests += 1
end

puts "#{tests} tests, #{errors} failed, #{ignored} ignored"
