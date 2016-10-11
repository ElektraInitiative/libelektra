#!/usr/bin/env ruby
## 
# @file 
# 
# @brief example Ruby application to illustrate usage of Elektras Ruby bindings
# 
# @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
# 
# 
# This example is a simple command line application, which illustrates the most 
# basic usage scenarios of the Elektras Ruby bindings. The application performs
# the following steps:
#  * open kdb database
#  * fetch all Elektra keys considered with this application
#  * look for a specific key and check if it really exists
#  * create a new Elektra key
#  * modify an existing key
#  * query and modify a keys metadata
#
# To run this example you have to install Elektras Ruby bindings or add the 
# path, under which the compiled Elektra Ruby library can be found to your 
# 'RUBYLIB' environment variable.
#
#  $> RUBYLIB="<path to the kdb.so>" ruby ruby_example_application.rb
#

require 'kdb'

# define a global app config namespace
APP_NS = "user/tests/ruby/exampleapp/#1/current"

#
# helper methods
#

def pretty_print_key( key )
    puts "key '%s'" % key.name
    if key.is_binary?
        puts "  binary key, lenght: %d" % key.value.length
        puts "     value: %x" % key.value
    else
        puts "  string value: %s" % key.value
    end
    if key.meta.empty?
        puts "  key has no meta data"
    else
        puts "  key meta data:"
        key.meta.each do |k|
            puts "      %s => %s" % [k.name, k.value]
        end
    end
end

def user_wants( question, default = "Y" )
    puts question
    begin
        print "Y/N [%s]: " % default
        answer = gets.chomp.upcase
        answer = default unless not answer.empty?
    end while not ["Y", "N"].include? answer
    
    answer == "Y"
end



#
# application
#


puts "Welcome to the Ruby-kdb example application\n\n"

# create a Kdb handle
begin
    db = Kdb::KDB.new
rescue Kdb::KDBException
    puts "could not open KDB database: %s" % $!
    exit 1
end

# to get our desired config settings we have to create a keyset first
ks = Kdb::KeySet.new

begin
    # get all config keys under our application settings path
    ret = db.get ks, APP_NS
rescue Kdb::KDBException
    puts "could not get Keys under %s: %s" % [APP_NS, $!]
end

# ret will be >= 0, if we successfully retrieved some keys

# now we can work with this keyset to get/set keys and their values
puts "looking for 'setting1'"

setting1 = ks.lookup "#{APP_NS}/setting1"
if setting1.nil?
    if not user_wants "config setting 'setting1' does not exist, create it?"
        puts "then there's nothing to do for me..."
        exit 0
    end
    # our requested setting does not exist
    # so create it
    setting1 = Kdb::Key.new "#{APP_NS}/setting1", value: "(empty)"
    # and add it to the keyset 
    ks << setting1
end

pretty_print_key setting1

if user_wants "update setting1?"
    print "new value: "
    new_value = gets.chomp
    # now we can change the settins properties as we like
    setting1.value = new_value
end

if user_wants "add meta data?"
    print "meta data name: "
    name = gets.chomp
    print "meta data value: "
    value = gets.chomp


    # add/modify some meta data
    setting1[name] = value
end

puts "Ok, now its time to set the changes and exit"

begin
    # now we are ready to save the updated settings
    ret = db.set ks, APP_NS
rescue KDBException
    puts "clould now update KDB database: %s" % $!
ensure
    db.close
end

# finally we close the our KDB handle
db.close

puts "done. Have a nice day"

# vi: sw=4 ts=4 sts=4 ai et
