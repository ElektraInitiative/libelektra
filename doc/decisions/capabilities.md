# Capabilities

## Issue

Every backend, except `filesys`, was unable to represent full file system
semantics.  Capabilities described the differences between `filesys`
and another backend. (For POSIX file systems a similar technique is
`pathconf()`. It allows the user to query the capabilities of a specific
mounted file system given by path.)  Capabilities made it possible
to implement a backend different from the way `filesys` works and let
the backend still have predictable behaviour.  The user could query a
backend if it was capable of a specific detail of file system semantics.

Capabilities were initially introduced to make backend development easier,
because they also expressed the disabilities of a backend.  For example,
it was possible for a backend to claim that it is not aware of comments.

Getting a single key works well for `filesys`.  However,
for configuration files the whole content must be parsed even for a
single key.  Capabilities were able to describe that the backend will
always retrieve and store all keys and will not be able to retrieve and
store individual keys.  This restriction simplified the implementation of
such backends considerably.  Capabilities allowed us to implement fstab,
passwd and hosts backends.

But we soon found the limits of capabilities.
Capabilities were unable to describe:

- that some key names are not allowed.
- that not every structure of configuration is allowed.
- that some characters are not allowed.
- that only specific key values are allowed.

## Constraints

## Assumptions

## Considered Alternatives

## Decision

No capabilities are used, but instead other plugins implement the
limitations other plugins have.

## Argument

The main problem was that the complexity was just moved to the
applications.  In the end, application developers had to understand
what the backend can write out.  They no longer had confidence that
the storage was able to understand the full semantics.  Elektra's core
was unable to hide that fact.  Capabilities did not turn out to make
backend development much easier.  Instead, Elektra was more difficult
to use with capabilities.  Capabilities are no longer part of Elektra
for these reasons.

## Implications

## Related decisions

## Notes
