#!/usr/bin/env ruby
#
# This is a simple script to test the Ruby -> Elektra -> Ruby plugin case
#
# preparation:
# kdb mount /test /ruby/test ruby script=<path to this dir>/simple_get.rb

require 'kdb'

1000.times do |i|
  puts "cycle #{i}"
  Kdb.open do |db|
    ks = Kdb::KeySet.new
    db.get ks, '/ruby/test'
    ks.each do |key|
      #puts key.name
    end
  end
end
