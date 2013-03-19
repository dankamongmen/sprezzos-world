$: << 'lib' << 'test' << '.'
EXC_TESTS = [
  'test/rubygems/functional.rb',
  'test/rubygems/insure_session.rb',
]

require 'rubygems'
require 'minitest/autorun'
(Dir['{spec,test}/**/test_*.rb'] - EXC_TESTS).each { |f| require f }
