# New API by-reference

### Note on structs

The public structs `ElektraName` and `ElektraValue` defined here are not problematic or limiting in terms of forward compatibility.
This is because, they essentially define what a "keyname" and a "key value" are for Elektra.
That is why they are used both as part of the public API **and** in the definitions of the `*Cow` structs, which define how stuff is actually stored.
Any change to these structs would mean changing that definition and therefore would be major breaking change, even if the structs where not public.

Using the structs this way, means we cannot change the internal storage of `ElektraName` or `ElektraValue`.
It would not be possible to e.g., store very short names directly in `ElektraName` instead of allocating a separate `char *` buffer.
The API imposes that an addressable instance of `ElektraName` must exist for every `ElektraEntry`.
