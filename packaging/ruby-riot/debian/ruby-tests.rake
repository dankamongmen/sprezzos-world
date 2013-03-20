task :default   => ["test:core"]
#task :default   => ["test:all"]
task "test:all" => ["test:core", "test:extensions"]

require 'rake/testtask'
Rake::TestTask.new("test:core") do |test|
  test.libs    << 'test'
  #test.pattern =  'test/core/**/*_test.rb'
  test.pattern =  'test/core/**/*_test.rb'
end

Rake::TestTask.new("test:extensions") do |test|
  test.libs    << 'test'
  test.pattern =  'test/extensions/*_test.rb'
end
