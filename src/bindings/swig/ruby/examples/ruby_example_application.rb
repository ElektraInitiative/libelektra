#!/usr/bin/env ruby
##
# @file
#
# @brief example Ruby application to illustrate usage of Elektra’s Ruby bindings
#
# @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
#
#
# This example is a simple command line application, which illustrates the most
# basic usage scenarios of Elektra’s Ruby bindings. The application performs
# the following steps:
#  * open kdb database
#  * fetch all Elektra keys considered with this application
#  * look for a specific key and check if it really exists
#  * create a new Elektra key
#  * modify an existing key
#  * query and modify a keys metadata
#
# To run this example you have to install Elektra’s Ruby bindings or add the
# path, under which the compiled Elektra Ruby library can be found to your
# 'RUBYLIB' environment variable.
#
#  $> RUBYLIB="<path to the kdb.so>" ruby ruby_example_application.rb
#

require 'kdb'

# define a global app config namespace
APP_NS = "/tests/ruby/exampleapp/#1/current"

# this is the basename of our key we want to manipulate
KEY_BASENAME = "setting1"

#
# helper methods
#

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
    db = Kdb.open
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
puts "looking for '#{KEY_BASENAME}'"

# first we check if there exists a key with that name already
# Note:
#   APP_NS starts with a '/', so we will perform a CASCADING lookup. This means
#   that Elektra will try to find our key in several places (namespaces). We
#   will get the first found key.
#   For details see https://github.com/ElektraInitiative/libelektra/blob/master/doc/tutorials/cascading.md
#
setting1 = ks.lookup "#{APP_NS}/#{KEY_BASENAME}"
if setting1.nil?
    if not user_wants \
            "config setting '#{KEY_BASENAME}' does not exist, create it?"
        puts "then there's nothing to do for me..."
        exit 0
    end
    # our requested setting does not exist
    # so create it
    setting1 = Kdb::Key.new "#{APP_NS}/#{KEY_BASENAME}", value: "(empty)"
    # and add it to the keyset
    ks << setting1
end

setting1.pretty_print


if user_wants "update #{KEY_BASENAME}?"
    # we did a cascading lookup, so we might got a system key
    if setting1.is_system? and user_wants \
            "This is a system key. You might have insufficient permissions."\
            "Create a user key instead?"

        # clone the key
        setting1 = setting1.clone
        # update its name
        setting1.name = "user:/#{APP_NS}/#{KEY_BASENAME}"
        # and finally add it to our keyset
        ks << setting1
    end

    print "new value: "
    new_value = gets.chomp
    # now we can change the settings properties as we like
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

puts "we now have:"
setting1.pretty_print

puts "Ok, now its time to set the changes and exit"

begin
    # now we are ready to save the updated settings
    ret = db.set ks, APP_NS
rescue Kdb::KDBException
    puts "could not update KDB database: %s" % $!
end

# finally we close the our KDB handle
db.close

puts "\ndone. Have a nice day"

# vi: sw=4 ts=4 sts=4 ai et
