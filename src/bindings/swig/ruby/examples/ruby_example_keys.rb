#!/usr/bin/env ruby
##
# @file
#
# @brief example Ruby application to illustrate usage of Elektras Ruby bindings
#
# @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
#
#
# This example illustrates the Kdb::Key aspects of Elektras Ruby bindings.
#
# To run this example you have to install Elektras Ruby bindings or add the
# path, under which the compiled Elektra Ruby library can be found to your
# 'RUBYLIB' environment variable.
#
#  $> RUBYLIB="<path to the kdb.so>" ruby ruby_example_keys.rb
#

# include libelektra 'kdb' module
require 'kdb'


# create a new empty key
k = Kdb::Key.new

# create a new key with initail name
k = Kdb::Key.new "user:/myapp/#1/config1"

# create a new fully initialized key
k = Kdb::Key.new("user:/myapp/#1/config1",
                  value: "some value",
                  meta_data: "important info",
                  owner: "me")

# create a new Key with special flags
k = Kdb::Key.new("user:/myapp/#1/bconfig",
                  flags: Kdb::KEY_BINARY)



# set a name
begin
  k.name= "user:/myapp/#1/config1"
rescue Kdb::KeyInvalidName
  puts "invalid key name given"
end

# get name methods
puts "k.name:      #{k.name}"
puts "k.basename: #{k.basename}"
puts "k.fullname: #{k.fullname}"
puts "k.namespace: #{k.namespace}"

# name manipulations
begin
  puts "k.add_name"
  k.add_name "../config2"
  puts "k.name: #{k.name}"

  puts "k.add_basename"
  k.add_basename "width"
  puts "k.name #{k.name}"
rescue Kdb::KeyInvalidName
  puts "invalid key name given"
end

# set a value
k.value= "120 px"

# get value
puts "k.value: #{k.value}"


#
# working with binary keys
#

# create an initially binary key
kbin = Kdb::Key.new("user:/myapp/#1/binkey", flags: Kdb::KEY_BINARY)
# can be tested
puts "kbin.is_binary?: #{kbin.is_binary?}"

# use the same value methods
kbin.value = "\000\001\002\003"
v = kbin.value
puts "kbin value: #{v.unpack("H*").first}"

# you can also use the get_binary, however be careful, calling this method
# on a non-binary key will throw a Kdb::KeyTypeMismatch exception
v = kbin.get_binary


# if key is not initially binary
kbin2 = Kdb::Key.new "user:/myapp/#1/binkey2"

# use the set_binary method to set a binary value. After this call
# the key will be a binary key
kbin2.set_binary "\000\001\002"

kbin2.is_binary? # => true



#
# working with metadata
#

# create a new key with initially set meta data
kmeta = Kdb::Key.new("uset/myapp/config/#1/c1",
                     owner: "me",   # meta data
                     # no meta, will set the value of the key
                     value: "some value",
                     # meta data
                     created: "some years ago",
                     importance: "high",
                     some_thing_stupid: "...")

# adding meta data to a Key
kmeta.set_meta "another meta", "value"
# there's also an Array-like access method
kmeta["jet another"] = "jet value"

# accessing meta data
mv = kmeta.get_meta "another meta"
mv = kmeta["jet another"]

# check if meta data exists
kmeta.has_meta? "jet another"  # => true

# delete meta data
kmeta.del_meta "jet another"


#
# metadata iterator
#

# ruby style iteration
kmeta.meta.each do |e|
  puts "kmeta metadata: #{e.name} => #{e.value}"
end

long_names_metadata = kmeta.meta.find_all { |e| e.name.size >= 8 }
# here we get a Ruby Array containing meta data keys, which meet the above
# criteria
long_names_metadata.each do |e|
  puts "kmeta long name metadata: #{e.name} => #{e.value}"
end


