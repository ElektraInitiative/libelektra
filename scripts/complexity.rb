# @author Anton Hößl <e1427045@student.tuwien.ac.at>
# @brief This script approximates the complexity of a elektra configuration namespace
# @date 18.04.2018
# @tags generator

require 'kdb'

# function checks if the type of the key is known and returns the number of bits needed to represent all possible values
# of that key, returns 1 if it is a unknown type. Doesn't consider range checks etc...
def evaluate_type(t, tc, k)
  if tc.key?(t) then return tc[t] end
  value = k.get_meta("check/enum")
  if t == "enum" && value != nil then
    
    if value.start_with?("#") then 
      # case of meta array
      # get number of enums, build log base 2 and round it to the nearest integer
      return Math.log2(Integer(value[1..-1])).ceil 
    else
      # case of enums defined as a string
      return Math.log2(value.count(",") + 1).ceil
    end
    
  end
  return 1
end

if ARGV.count != 1 then
  p 'Usage: complexity.rb NAMESPACE'
  exit 1
end

APP_NS = ARGV[0]

puts "Welcome to the Ruby Complexity script, calculating complexits for namespace:\n"
print APP_NS + "\nThis might take a while."

# create a Kdb handle
begin
  db = Kdb.open
rescue Kdb::KDBException
  puts "could not open KDB database: %s" % $!
  exit 1
end
print "."
# to get our desired config settings we have to create a keyset first
ks = Kdb::KeySet.new
begin
  # get all config keys under our application settings path
  ret = db.get ks, APP_NS
rescue Kdb::KDBException
  puts "could not get Keys under %s: %s" % [APP_NS, $!]
end
print ". (#{ks.size} keys found) \n"

numbers_of = Hash.new(0);

type_complexity = Hash.new(1); # complexity for data-types in number of bits
type_complexity["short"] = 8;
type_complexity["unsigned_short"] = 8;
type_complexity["long"] = 32;
type_complexity["unsigned_long"] = 32;
type_complexity["long_long"] = 64;
type_complexity["unsigned_long_long"] = 64;
type_complexity["float"] = 32;
type_complexity["double"] = 64;
type_complexity["long_double"] = 128;
type_complexity["char"] = 8;
type_complexity["octet"] = 8;
type_complexity["boolean"] = 1;
type_complexity["any"] = 1;
type_complexity["empty"] = 1;
type_complexity["string"] = 1;

complexityPower = 0;

# go through all keys and process them
ks.each { |k|
  found_meta_type = false
  k.meta.each do |m|
    if m.name === "check/type" || m.name === "type" then
      numbers_of[m.value] += 1;
      complexityPower += evaluate_type(m.value, type_complexity, k)
      found_meta_type = true
      break
    end
  end
  
  if !found_meta_type then
    numbers_of['any'] += 1;
    complexityPower += evaluate_type('any', type_complexity, k)
  end
}

puts "\n\n"
numbers_of.each {|key, value| puts "#{key} occured #{value} times."}
puts "\n\n"
puts "Total complexity is approximately 2^#{complexityPower}"

db.close()