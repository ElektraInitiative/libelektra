# [CM P0] Replace internal iterators with external iterators (@flo91, @Milangs)

The internal iterator must be removed and replaced with the external iterator.

# Interactions with users

Developers of plugins should already use the external iterators instead of the internal ones. However, following functions will be removed:

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

Every occurence of using the internal iterator has to be replaced with the external iterator (see #4281).
The functions of the internal iterator are listed in #3171.
There are also some uses of the internal iterator found in the SWIG bindings (Lua, Ruby, Python) that also need removal (see #4279).

# Examples

An example for using an external iterator is:

```c
for (elektraCursor it = 0; it < ksGetSize (ks); ++it)
{
    Key * cur = ksAtCursor (ks, it);
    // [loop body]
}
```

You can obtain the key at a specific position in the keyset and the overall size of the keyset.
That should be all you need for iterating over keys.
For future releases, the function `ksAtCursor` will be renamed to `ksAt`. (see issue #3976)

# Work distribution

Basically, we plan to work together on all parts of the project.
However, the main focus of Michael Langhammer will be #4281 and #4279, while Florian Lindner will focus on #4280.
