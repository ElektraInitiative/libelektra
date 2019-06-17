# Error message

Elektra has a lot of developers and hence the error messages vary in how they are written.
This guideline should unify the message format so that users will get consistent error messages.

- All error messages start with a capital letter even if you reference a method name such as "KdbSet() not supported". The only exception are quoted messages at the start such as "'xxxx-xxxx' is an invalid syntax". Try to avoid variable strings at the start of the sentence ("%s"-strings) if possible because you cannot guarantee capital casing.
- **Always** provide the errno if possible.
- If you can provide an errno, write `"Reason: %s", strerror(errno)` at the end of the error message. Also end the preceding sentence with a dot. Example: `"XY failed. Reason: %s", strerror(errno)`. Alternatively you can also embed it into a standard text: `"XY failed because of %s", strerror(errno)`.
- The last sentence (or single sentence of your message if you just provide one) must not end with a dot. This should encourage users to continue reading
if other messages appear.
- If your message has multiple sentences, separate them with dots and start with a capital letter like you would do in the normal english language.
- Avoid abbreviations because not everybody will know them (eg. "No Byte Order Mark found" instead of "No BOM found").
- Never use exclamation marks at the end of sentences.
- If it is unclear where embedded strings in error messages start and end ("%s"-strings), surround them by single quotes '. Sentences containing an errno and end with `Reason: %s` do not need extra quotes because it is clear where they start and end.