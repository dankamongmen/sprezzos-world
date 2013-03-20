if RUBY_VERSION <= '1.9.1'
  system("ruby1.8 debian/ruby-bacon/usr/bin/bacon --automatic --quiet") or raise
else
  system("ruby1.9.1 debian/ruby-bacon/usr/bin/bacon --automatic --quiet") or raise
end
