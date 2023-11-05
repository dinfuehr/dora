#!/usr/bin/ruby

id = 0

loop do
    id_name = "%06d" % id
    files = Dir["fuzz-out/default/crashes/id:#{id_name},*"]
    break if files.length != 1
    file = files[0]
    puts "check #{file}"

    system("cargo run -p dora -- --check #{file}")
    result = $?

    if result.exitstatus > 1
        break
    end

    id = id + 1
end
