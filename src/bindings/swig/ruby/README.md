# Ruby bindings #

This module is a SWIG generated binding for KDB (http://www.libelektra.org),
therefore the module provides wrapper classes to KDBs C++ interface and is
mainly a 1 to 1 relation. However, to provide a more Ruby-style API to KDB,
this module differs to the C++ API in the following way:
 * C++ iterators for Key/KeySet are excluded. Instead KeySet implements
   a 'each' method and includes 'Enumerable'. Therefore it is very similar to
   a Ruby-Array. However, the KeySet cursor methods are still available.
 * Access to native C-level KDB structures (such as ckdb::Key) is not
   possible, as this does not make much sense within Ruby.
 * Method names are renamed to follow Ruby naming conventions
 * Key and KeySet methods directly modify the underlying Key/KeySet
 * The `Key.setCallback` and `Key.getFunc` methods are not supported

Below, we show some specifics of the Ruby binding.

## Quick start guide ##

This is a short example and illustrates how to use the binding:
	
	require 'kdb'
	
	Kdb.open do |db|
		ks = Kdb::KeySet.new
		db.get ks, '/'
		
		k = ks.lookup "/org/sw/myapp/#1/current/setting1"
		if !k.nil?
			k.value = "new value"
			k['some metadata'] = "additional info"

			k.pretty_print
		end

		db.set ks, '/'
	end

## Get a KDB handle ##

There are two ways to create a KDB handle:

Creating a new KDB instance:

	db = Kdb::KDB.new
	# requires a db.close afterwards

or with the `open` method:
	
	db = Kdb.open
	# requires a db.close afterwards

This `open` method supports also a block:
	
	Kdb.open do |db|
		...
	end
	# db.close is called implecitly

Note: after the block was executed, `db.close` is called implicitly, Thus you 
can not use the handle afterwards.

## Exception handling ##

Exception handling is directly mapped from the C++ binding:
	
	begin
		db = Kdb.open
		...
	rescue Kdb::KDBException
		puts "something went wrong: %s" % $!
	ensure
		db.close
	end

## KeySet creation ##

A KeySet can be created in different ways:

An empty key set:
	
	ks = Kdb::KeySet.new

A key set with one initial key:
	
	ks = Kdb::KeySet.new Kdb::Key.new("user/sw/key1")

Passing an array of initial keys:
	
	ks = Kdb::KeySet.new [
		Kdb::Key.new "user/sw/key1"
		Kdb::Key.new "user/sw/key2"
		Kdb::Key.new "user/sw/key3"
	]

Passing a KeySet of initial keys:
	
	ks2 = Kdb::KeySet.new ks


## appending keys ##

`KeySet` supports Ruby's "append" operator `<<`
	
	ks << Kdb::Key.new("user/sw/key1")

## iteration ##

`KeySet` has an `each` method and includes `Enumerable`
	
	ks.each { |k| puts k.to_s }
	ks.any? { |k| k.has_meta? "owner" }

## deleting Keys ##

Similar to the Ruby `Array`, `KeySet` also implements `delete` and `delete_at`
methods, which can be used to delete a key by name or by index:
	
	ks.delete "user/sw/key1"
	ks.delete_at 2

## Key creation ##

Keys can be created with a Hash-like variable argument list:
	
	Kdb::Key.new "user/sw/key1", value: "v1", owner: "me"

The first argument is the name of the `Key`, followed by an optional list of 
symbol value pairs. The symbol value pairs are interpreted as follows:
- `value`: this will set the value of the newly created key
- `flags`: pass a ORed list of Key flags
- any other symbol is interpreted as meta data key and therefore will add a new
  metadata key.

## Key meta data iteration ##

This is very similar to the `KeySet` iteration and can be accessed with `meta`:
	
	k.meta.each do |m|
		puts "#{m.name}: #{m.value}"
	end
