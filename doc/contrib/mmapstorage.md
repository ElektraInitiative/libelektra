# mmapstorage

The `mmapstorage` plugin is a high performance storage plugin that supports full Elektra semantics.

The most important constraint for `mmapstorage` is that any structure (or bytes) that is an allocation unit (e.g. we `malloc()` the bytes needed for `KeySet` struct, so this is an unit) needs to have a flag to determine whether those bytes are actually `malloc()`ed or they are `mmap()`ed.

The `mmapstorage` plugin only calls `munmap` in some error cases, so basically the returned keyset is never invalidated and `munmap` is never called for its data.

During `kdbSet` the storage plugins always write to a temp file, due to how the resolver works.
We also don't need to `mmap` the temp file here: when doing `kdbSet` we already have the `KeySet` at hand, `mmap`-ing it is not needed at all, because we have the data.
We just want to update the cache file.
The `mmap`/`munmap` in `kdbSet` are just so we can write the `KeySet` to a file in our format.
(`mmap()` is just simpler, but we could also `malloc()` a region and then `fwrite()` the stuff)

Therefore the only case where we return a `mmap()`ed KeySet should be in `kdbGet`.

When the `mmapstorage` was designed/implemented, not all structures had refcounters, so there was no way to know when a `munmap` is safe.
This was simply out of scope at that point in time.
