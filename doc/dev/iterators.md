# Migrating from internal to external KeySet iterators

The deprecated internal iterator are removed and replaced with external iteration.

# Interactions with users

Developers of plugins should already use the external iterators instead of the internal ones.
However, following functions will be removed:

```C
int ksRewind (KeySet *ks);
Key *ksNext (KeySet *ks);
Key *ksCurrent (const KeySet *ks);

elektraCursor ksGetCursor (const KeySet *ks);
int ksSetCursor (KeySet *ks, elektraCursor cursor);

int keyRewindMeta (Key *key);
const Key *keyNextMeta (Key *key);
const Key *keyCurrentMeta (const Key *key);
```

# The Architecture

Every occurrence of using the internal iterator has to be replaced with the external iterator (see #4281).
The functions of the internal iterator are listed in #3171.
There are also some uses of the internal iterator found in the SWIG bindings (Lua, Ruby, Python) that also need removal (see #4279).

## Internal vs. external Iterators

When the client (i.e. the programmer) controls the iteration, the iterator is called external iterator.
Otherwise, when the iterator controls the iteration, it is called internal iterator.
Due to the fact that external iterators are more flexible the internal iterators are the way to go.

# Examples

An example for using an external iterator is:

```c
for (elektraCursor it = 0; it < ksGetSize (ks); ++it)
{
    Key * cur = ksAtCursor (ks, it);
    // [loop body]
}
```

You can obtain the `Key` at a specific position in the `KeySet` and the overall size of the `KeySet`.

If you want to delete `Key`s during the iteration of a `KeySet`, be aware that all keys after the
deleted `Key` are moved one slot forward, so maybe you have to change to value of `it` after deleting
a `Key`:

```c
for (elektraCursor it = 0; it < ksGetSize (ks); ++it)
{
    Key * cur = ksAtCursor (ks, it);
    if ( shouldKeyGetDeleted (cur))
    {
	    keyDel (cur);
	    --it; //next key is now at the position of the current key which was deleted
    }
}
```

For such scenarios, it is also important that you recalculate the size with `ksGetSize ()`
within the loop-header or explicitly after changing the `KeySet`, e.g. by deleting a `Key`.

That should be all you need for iterating over keys.
For future releases, the function `ksAtCursor` will be renamed to `ksAt`. (see issue #3976)

You can iterate over metakeys in a similar fashion as long as you iterate over a `KeySet`, e.g., `keyMeta("my_key")` returns a `KeySet` of metakeys.

The following is a comprehension of how to use iterators in various languages.

### C

```c
for (elektraCursor it = 0; it < ksGetSize (ks); ++it)
{
    Key * cur = ksAtCursor (ks, it);
    // ...
}
```

### C++

C-style fashioned loop:

```cpp
for (elektraCursor it = 0; it < ks.size (); ++it)
{
	Key key = ks.at(it);
	// ...
}
```

Real iterators the C++ interface supports:

```cpp
for (KeySet::iterator i = ks3.begin(); i != ks3.end(); ++i)
{
	Key key(*i);
	// ...
}
```

### Python

```python
size = ksSize(keySet)
for cursor in range(size):
	key = ksAt(keySet, cursor)
	# ...
```

### Lua

```lua
size = kdb.ksSize(keySet);
if size > 0 then
	for i = 1, size do
		key = kdb.ksAt(keySet, i - 1)
		-- ...
	end
end
```

### Ruby

```ruby
keySet.each do |key|
	# ...
end
```
