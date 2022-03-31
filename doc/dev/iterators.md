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

<TODO>
