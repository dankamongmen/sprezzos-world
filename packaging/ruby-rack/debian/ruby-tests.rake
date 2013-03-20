# -*- mode: ruby; coding: utf-8 -*-
require 'rbconfig' unless defined? RbConfig
ruby = File.join(RbConfig::CONFIG['bindir'], RbConfig::CONFIG['ruby_install_name'])

task :default do
  sh "#{ruby} /usr/bin/bacon -I./lib:./test -w -a -q -t '^(?!Rack::Adapter|Rack::Session::Memcache|Rack::Server|Rack::Handler)'"
end
