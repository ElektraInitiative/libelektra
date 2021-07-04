# keyString() return value

## Problem
When using keyString() on empty / binary values the return values are the literal strings (null) / (binary). This seems very awkward and unintuitive from a user's perspective.

## Constraints
Because the return value of the function is const char*, there are not so many possibilities for a change to this behaviour, as every possible return value - except for a NULL pointer - could just be the value of the string of the Key. Possible changes to this behaviour would be:

## Assumptions
On one hand always returning a string allows things like `strlen(keyString(k)) == 0` to check for an empty string. But at the same time people might expect that `strlen(keyString(k)) < MAX_LEN` also works, but this might silently break code down the line, if `k` is binary and `strlen("(binary)") < MAX_LEN`.

## Considered Alternatives
An alternative would be to always store a zero byte after all key values, even if they are binary (might be done already). Then we can safely return `k->data.c == NULL ? "" : k->data.c`. It may contain incomplete data and the `MAX_LEN` problem from above still applies, but there are no segfaults and you don't get return values that have nothing to do with the actual data.

## Decision
- Obviously just return the string value, if the key is a non-empty string key.
- Return NULL, if the key is binary (no matter what the value is)
- Return an empty string, otherwise, i.e. when key contains an empty string or k->data.c == NULL, but the key is not marked as binary.

## Rationale
This way you'll probably get a segfault for unexpected binary keys (unless you check for them), but the common cases are still easy to handle without much checking. I prefer the segfault over returning some fixed string value, because then bugs don't cause silent problems.

## Implications

## Related Decisions

## Notes
