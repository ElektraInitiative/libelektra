- infos =
- infos/author = Bernhard Denner <bernhard.denner@gmail.com>
- infos/status = maintained
- infos/provides = swig
- infos/description =

# Ruby Bindings

This module is a SWIG generated Ruby binding for Elektra Core API - KDB
(https://www.libelektra.org) and its libtools library. So it consists of the two
Ruby modules:

- `Kdb` wrapping Elektra's core API
- `Kdbtools` wrapping Elektra's libtools API

The two modules provides wrapper classes the C++ interface and are
mainly a 1 to 1 mapping. However, to provide a more Ruby-style API,
the modules differs to the C++ API in the following way:

## Installation

See [installation](/doc/INSTALL.md).
The package is called `ruby-elektra`.

## Ruby `Kdb` Differences to C++ API

- C++ iterators for `Key`/`KeySet` are excluded. Instead `KeySet` implements
  an `each` method and includes `Enumerable`. Therefore it is very similar to
  a Ruby-Array.
- Method names are renamed to follow Ruby naming conventions
- Access to native C-level `KDB` structures (such as `ckdb::Key`) is not
  possible, as this does not make much sense within Ruby.
- Key and `KeySet` implement a `pretty_print` method very suitable for debugging
- The `Kdb` module has a static `open` method taking a block, which can be
  used to access the opened KDB handle within a block, whereas the handle is
  closed afterwards (similar to `File.open`).
- `Key.new` accepts an Hash-like argument list, imitating the C++ variable
  arguments feature. However, we do not relay on the `KEY_END` flat to
  indicate the end of a list.
- Additional to `Key`s and `KeySet`s, the `KeySet`.new method also accepts a
  Ruby-Array of `Key`s for `KeySet` creation. This allows a very short
  KeySet creation.
- `KeySet` implements the usual `<<` operator for appending `Key`s and
  `KeySet`s.
- `Key` and `KeySet` methods directly modify the underlying `Key`/`KeySet`
- The `Key.setCallback` and `Key.getFunc` methods are not supported

## Ruby `Kdbtools` Differences to C++ API

These bindings do not really change anything to the API and are basically a
direct 1:1 mapping. However the usual renaming from C++ camelcase to Rubys
naming conventions also was done here.

- methods for fetching library symbol function pointers are excluded

## Examples

Example and demo applications which a lot of documentation in it can be found
in the Ruby-bindings source tree under 'examples'. Also the test cases (under
'tests') can be a good point to look if you need more information.

## Quick Start Guide

This is a short example and illustrates how to use the binding:

```ruby
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
```

## Get a KDB Handle

There are two ways to create a KDB handle:

Creating a new KDB instance:

```ruby
db = Kdb::KDB.new
# requires a db.close afterwards
```

or with the `open` method:

```ruby
db = Kdb.open
# requires a db.close afterwards
```

This `open` method supports also a block:

```ruby
Kdb.open do |db|
	...
end
# db.close is called implicitly
```

Note: after the block was executed, `db.close` is called implicitly, Thus you
can not use the handle afterwards.

## Exception Handling

Exception handling is directly mapped from the C++ binding:

```ruby
begin
	db = Kdb.open
	...
rescue Kdb::KDBException
	puts "something went wrong: %s" % $!
ensure
	db.close
end
```

## KeySet Creation

A KeySet can be created in different ways:

An empty key set:

```ruby
ks = Kdb::KeySet.new
```

A key set with one initial key:

```ruby
ks = Kdb::KeySet.new Kdb::Key.new("user:/sw/key1")
```

Passing an array of initial keys:

```ruby
ks = Kdb::KeySet.new [
	Kdb::Key.new "user:/sw/key1"
	Kdb::Key.new "user:/sw/key2"
	Kdb::Key.new "user:/sw/key3"
]
```

Passing a KeySet of initial keys:

```ruby
ks2 = Kdb::KeySet.new ks
```

## Appending Keys

`KeySet` supports Ruby's "append" operator `<<`

```ruby
ks << Kdb::Key.new("user:/sw/key1")
```

## Iteration

`KeySet` has an `each` method and includes `Enumerable`

```ruby
ks.each { |k| puts k.to_s }
ks.any? { |k| k.has_meta? "owner" }
```

## Deleting Keys

Similar to the Ruby `Array`, `KeySet` also implements `delete` and `delete_at`
methods, which can be used to delete a key by name or by index:

```ruby
ks.delete "user:/sw/key1"
ks.delete_at 2
```

## Key Creation

Keys can be created with a Hash-like variable argument list:

```ruby
Kdb::Key.new "user:/sw/key1", value: "v1", owner: "me"
```

The first argument is the name of the `Key`, followed by an optional list of
symbol value pairs. The symbol value pairs are interpreted as follows:

- `value`: this will set the value of the newly created key
- `flags`: pass a ORed list of Key flags
- any other symbol is interpreted as metadata key and therefore will add a new
  metadata key.

## Key Metadata Iteration

This is very similar to the `KeySet` iteration and can be accessed with `meta`:

```ruby
k.meta.each do |m|
	puts "#{m.name}: #{m.value}"
end
```

## Building

Building the Ruby bindings from Elektra src tree requires to have SWIG >= 3.0.8
installed. Additionally ruby header files (Debian/Ubuntu ruby-dev) are required
too.

The bindings where tested with Ruby >= 2.1.0, lower version might work but are
not tested.
