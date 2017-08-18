#!/usr/bin/ruby

errors = 0
tests = 0

for file in Dir["tests/**/*.dora"]
  if !system("target/debug/dora", "--check", file)
    puts "#{file} failed"
    errors += 1
  end

  tests += 1
end

puts "#{tests} tests, #{errors} failed"
