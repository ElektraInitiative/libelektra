# keyString() return value

## Problem

When using keyString() on empty / binary values the return values are the literal strings (null) / (binary).
This seems very awkward and unintuitive from a user's perspective.

## Constraints

Because the return value of the function is `const char*`, there are not so many possibilities for a change to this behavior, as every possible return value - except for a NULL pointer - could just be the value of the string of the Key.

## Assumptions

On one hand always returning a string allows things like `strlen(keyString(k)) == 0` to check for an empty string.
But at the same time people might expect that `strlen(keyString(k)) < MAX_LEN` also works.
But this again might silently break code down the line, if `k` is binary and `strlen("(binary)") < MAX_LEN`.

## Considered Alternatives

An alternative would be to always store a zero byte after all key values, even if they are binary (might be done already).
Then we can safely return `k->data.c == NULL ? "" : k->data.c`.
It may contain incomplete data and the `MAX_LEN` problem from above still applies, but there are no segfaults and you don't get return values that have nothing to do with the actual data.

## Decision

- `key == NULL` return 0, error code via second channel
- `key->value == NULL` return 0, error code via second channel
- `key == <binary>` return 0, error code via second channel
- everything else as is

## Rationale

- 0 seems like the most intuitive value to return in case of an error, although this introduces the possibility of segfaults for users of the library.
  printf doesn't cause segfaults anymore, which improves this problem a lot.
- With the introduction of a second channel for reporting errors, users can check the error messages in case of segfaults - which alleviates this issue.
  The first thing in case of an error should be checking the error message.

## Implications

## Related Decisions

## Notes
