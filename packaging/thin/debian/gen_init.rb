#!/usr/bin/ruby -w

require 'erb'
require 'gem2deb/dh_ruby'
if RUBY_VERSION.match(/1.8/)
  require 'ftools'
  ftools_klass = File
else
  require 'fileutils'
  ftools_klass = FileUtils
end
class Command
  def self.script
    "/usr/bin/thin"
  end
end
INITD_PATH="/etc/init.d/thin"
rubies = {}
Gem2Deb::DhRuby::SUPPORTED_RUBY_VERSIONS.each{|x,y| x = x.sub(/^ruby/, ""); rubies[x] = y;}
rubies.keys.each do |x|
  ftools_klass.makedirs(File.join("debian", "thin", "etc", "thin" + x))
end
File.open(File.dirname(__FILE__) + '/thin.init', 'w') do |f|
  f << ERB.new(File.read("lib/thin/controllers/service.sh.erb")).result(binding)
end
