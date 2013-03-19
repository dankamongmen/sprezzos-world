$: << 'test'
Encoding.default_external = "UTF-8" if RUBY_VERSION >= '1.9.1'
#test/file/xinclude.xml not included in gem 1.5.2
(Dir['test/**/test_*.rb']-["test/xml/test_xinclude.rb"]).each { |f| require f }

