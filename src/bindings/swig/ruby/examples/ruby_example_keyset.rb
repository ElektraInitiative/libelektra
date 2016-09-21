

require 'kdb'


#
# creating keysets
#

# create an empty keyset
ks = Kdb::KeySet.new

# create a keyset with one initial key
ks = Kdb::KeySet.new Kdb::Key.new("user/myapp/#1/config")

# create a keyset with many initial keys
ks = Kdb::KeySet.new [
        Kdb::Key.new("user/myapp/#1/config1"),
        Kdb::Key.new("user/myapp/#1/config2"),
        Kdb::Key.new("user/myapp/#1/config3"),
        Kdb::Key.new("user/myapp/#1/config4")
]

# create a keyset from an already existing keyset
ks = Kdb::KeySet.new ks


#
# appending new keys
#

# append method
ks.append Kdb::Key.new("user/myapp/#1/setting1")

# shift-left operator
ks << Kdb::Key.new("user/myapp/#1/setting2")

# append a keyset
ks_append = Kdb::KeySet.new [
        Kdb::Key.new("user/myapp/#1/setting3"),
        Kdb::Key.new("user/myapp/#1/setting4")
]

ks << ks_append

# append an array of keys
ks << [ Kdb::Key.new("user/myapp/#1/setting5"),
        Kdb::Key.new("user/myapp/#1/setting6") ]


#
# accessing keys in the keyset
#

# at method
key = ks.at 0

# index operator
key = ks[0]

# getting the first and the last key
first = ks.head
last = ks.tail

# lookup keys in the keyset
key = ks.lookup "user/myapp/#1/setting1"
# can also be done by key
key = ks.lookup Kdb::Key.new("user/myapp/#1/setting2")


#
# removing keys from the keyset
#

# pop, get and removes the last key
key = ks.pop

# lookup, get and remove key by name/key
key = ks.lookup "user/myapp/#1/setting6", Kdb::KDB_O_POP


#
# checking the size of the keyset
#
count = ks.size 
count = ks.length

empty = ks.empty?


#
# iterating over keysets
#

# classic kdb iteration
ks.rewind
while ks.next do
        key = ks.current
end

# Ruby style iteration
ks.each do |k|
        puts "key #{k.name}"
end

# Kdb::KeySet includes Enumerable, thus all Enumerable methods are working too
found = ks.find_all { |k| k.has_meta? "owner" }
ks.any? { |k| k.is_binary? }

# key in the keySet can be modified during iteration
ks.each { |k|
        k.value= "all keys get the same value"
}


#
# comparision
#

ks_a = Kdb::KeySet.new Kdb::Key.new("user/key1")
ks_b = Kdb::KeySet.new Kdb::Key.new("user/key1")

ks_a == ks_b  # => true
ks_a != ks_b  # => false
ks_a.eql? ks_b  # => true

#
# cloning keySets
#

ks_clone = ks.clone
ks_clone = ks.dup

ks_clone == ks  # => true

ks_clone << Kdb::Key.new("user/newkey")

ks_clone == ks  # => false





