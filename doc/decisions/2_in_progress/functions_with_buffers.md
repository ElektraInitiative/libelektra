# Functions copying into buffers

## Problem

Currently the way functions like keyGetName() work is by passing a buffer with
a maxSize and if the buffer is large enough, the value gets copied into the
buffer. This leads to the user having to write a lot of surrounding boilerplate
code, checking for the size of every value / name they want to copy into a buffer.

## Constraints

## Assumptions

## Considered Alternatives

## Decision

- Remove Functions:
  - keyGetName()
  - keyGetUnescapedName()
  - keyGetBaseName()
  - keyGetString()
  - keyGetBinary()
- add documentation in API documentation about life-time and add in release notes that you should use strncpy() / memcpy() instead:

```c
// str values
strncpy(..., keyName (k), ...)
// binary values
memcpy(..., keyValue (k), ...)
```

## Rationale

The functions clutter the API and try to replace existing builtin functionality
for little to no gain. This makes the API leaner while also retaining its
functionality.

## Implications

## Related Decisions

## Notes
