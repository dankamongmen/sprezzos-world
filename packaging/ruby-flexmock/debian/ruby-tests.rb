require 'rbconfig'
ENV['RUBYOPT'] = '-I' + RbConfig::CONFIG['rubylibdir'] # give priority to test/unit from the stdlib
ruby = File.join(RbConfig::CONFIG['bindir'], RbConfig::CONFIG['ruby_install_name'])
exec("#{ruby} -S rake")
