require 'gem2deb/rake/testtask'
require 'rspec/core/rake_task'
require 'fileutils'
# task 'default' => [:spec, 'original-test']
task 'default' => :spec

Rake::TestTask.new('original-test') do |tasklib|
  tasklib.test_files = FileList['original-tests/*.tests.rb']
end


task_cls = RSpec::Core::RakeTask
rspec_opts_setting_proc = proc do |task|
  task.rspec_opts = ['-fs']
end
# Proc for initializing each spec-tasks by common parameters
setting = proc do |st|
  st.ruby_opts = %w(-Ku)
end
desc "Verify all spec files."
task_cls.new do |st|
  setting.call(st)
  st.pattern = 'spec/*.rb'
end

desc "Verify all spec files with specdocs."
task_cls.new(:specd) do |st|
  setting.call(st)
  rspec_opts_setting_proc.call(st)
  st.pattern = 'spec/*.rb'
end

namespace :spec do
  Dir.glob('spec/*.rb') do |path|
    desc "Verify '#{path}'"
    task_cls.new(File.basename(path, '.rb')) do |st|
      setting.call(st)
      st.pattern = path
    end
  end
end
