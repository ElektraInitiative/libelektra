# Store the escaped and/or unescaped key name

## Problem

Memory Consumption in Elektra is quite high as the key names
are long and stored twice in memory.

## Constraints

- comparing of keys and thus searching in key sets must be possible with memcmp

## Assumptions

- printing the escaped key name is not so much used

## Considered Alternatives

- store both

## Decision

Only store one key name, suitable for comparing/searching/iterating over name.

## Rationale

- saves memory
- reduces API (`keyUnescapedName*`)

## Implications

- rename `keyUnescapedName` to  `keyName`
- To get the keyName you then would need something like `elektraEncodeKeyName(buffer, keyName(...

## Related Decisions

## Notes
