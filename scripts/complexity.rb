#!/usr/bin/env ruby
# @author Anton Hoessl <e1427045@student.tuwien.ac.at>
# @brief This script approximates the complexity of Elektra configuration settings
# @date 18.04.2018
# @tags specification
#
# This script calculates the complexity of Elektra configuration settings.
# It does so by checking for the meta information 'type' and 'check/type'.
# All types of the type-plugin and enums are supported.
# If a given key doesn't have the required metadata its complexity is assumed 2 as either configured correct or not.
#
# The complexity is displayed in the form of 2^x.
#
# To run this script you have to install Elektra's Ruby bindings or add the
# path, under which the compiled Elektra Ruby library can be found to your
# 'RUBYLIB' environment variable.
#
#  $> RUBYLIB="<path to the kdb.so>" ruby complexity.rb NAMESPACE
#

require 'kdb'

# complexity for data-types in number of bits
TYPE_COMPLEXITY = Hash.new(1)
TYPE_COMPLEXITY['short'] = 8
TYPE_COMPLEXITY['unsigned_short'] = 8
TYPE_COMPLEXITY['long'] = 32
TYPE_COMPLEXITY['unsigned_long'] = 32
TYPE_COMPLEXITY['long_long'] = 64
TYPE_COMPLEXITY['unsigned_long_long'] = 64
TYPE_COMPLEXITY['float'] = 32
TYPE_COMPLEXITY['double'] = 64
TYPE_COMPLEXITY['long_double'] = 128
TYPE_COMPLEXITY['char'] = 8
TYPE_COMPLEXITY['octet'] = 8
TYPE_COMPLEXITY['boolean'] = 1
TYPE_COMPLEXITY['any'] = 1
TYPE_COMPLEXITY['empty'] = 1
TYPE_COMPLEXITY['string'] = 1

#
# Helper methods
#

# function checks if the type of the key is known and returns the number of bits needed to represent all possible values
# of that key, returns 1 if it is a unknown type. Doesn't consider range checks etc...
#
# @param [Object] type of the passed key
# @param [Object] key which should be processed, needed in case it doesn't have a known type or is an enum
# @return [Integer] number of bits needed to represent the complexity of the given key
#
def evaluate_type(type, key)
  return TYPE_COMPLEXITY[type] if TYPE_COMPLEXITY.key?(type)

  value = key.get_meta('check/enum')
  return unless type == 'enum' && !value.nil?

  # case of meta array
  # get number of enums, build log base 2 and round it to the nearest integer
  return Math.log2(Integer(value[1..-1])).ceil if value.start_with?('#')

  # case of enums defined as a string
  Math.log2(value.count(',') + 1).ceil
end

#
# application
#

if ARGV.count != 1
  p 'Usage: complexity.rb NAMESPACE'
  exit 1
end

APP_NS = ARGV[0]

puts "Welcome to the Ruby Complexity script, calculating complexity for namespace:\n"
print APP_NS + "\nThis might take a while."

# create a Kdb handle
begin
  db = Kdb.open
rescue Kdb::KDBException
  puts format('could not open KDB database: %<error>s', error: $ERROR_INFO)
  exit 1
end
print '.'
# to get our desired config settings we have to create a key-set first
ks = Kdb::KeySet.new
begin
  # get all config keys under our application settings path
  db.get ks, APP_NS
rescue Kdb::KDBException
  puts format('could not get Keys below %<app_name_space>s: %<error>s',
              app_name_space: APP_NS, error: $ERROR_INFO)
end
print ". (#{ks.size} keys found) \n"

numbers_of = Hash.new(0)

complexity_power = 0

# go through all keys and process them
ks.each do |k|
  found_meta_type = false
  k.meta.each do |m|
    next unless m.name == 'check/type' || m.name == 'type'
    numbers_of[m.value] += 1
    complexity_power += evaluate_type(m.value, k)
    found_meta_type = true
    break
  end

  unless found_meta_type
    numbers_of['any'] += 1
    complexity_power += evaluate_type('any', k)
  end
end

puts "\n\n"
numbers_of.each { |key, value| puts "#{key} occurred #{value} times." }
puts "\n\n"
puts "Total complexity is approximately 2^#{complexity_power}"

db.close
