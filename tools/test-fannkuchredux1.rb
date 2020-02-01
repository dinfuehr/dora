$RUNS = 20

success = 0
failure = 0

system("cargo build")

$RUNS.times.each do
    result = system("ruby tools/tester.rb")

    if result
        success += 1
    else
        failure += 1
    end
end

puts "success=#{success} failure=#{failure}"
exit 1 if failure > 0
