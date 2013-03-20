# This file must be (`ruby debian/test.rb`) once the libshoulda-ruby* packages
# are installed to make some sanity checks, and to verify that they work, at least
# at a basic level.
require 'test/unit'
require 'shoulda'

class NiceTest < Test::Unit::TestCase

  context "shoulda" do

    should 'just work' do
      # is this test runs at all it already works :)
      assert true
    end

    context "in Debian" do

      should 'move proc_extensions.rb out of top level ruby library dir' do
        assert !File.exists?('/usr/lib/ruby/1.8/proc_extensions.rb')
        assert File.exists?('/usr/lib/ruby/1.8/shoulda/proc_extensions.rb')
      end

      should 'install RDOC documentation' do
        assert File.exists?('/usr/share/doc/libshoulda-ruby/rdoc/index.html'), 'is libshoulda-ruby package installed?'
      end

    end

  end
  
end
