# Error Message

Elektra has a lot of developers which leads to error messages written in various ways.
This guideline aims to unify the format of messages so that users see consistent error messages.

- All error messages start with a capital letter. Reword sentences that theoretically require a lowercase first letter such as
  regex patterns, method names or variable strings ("%s"-strings) to have a normal word at the start of the sentence. Also surround method names with single quotes.
  Example: `kdbSet() not supported` should be changed to `Method 'kdbSet()' is not supported`
- Always report all information which could be useful for the user. For some error types, templates are provided to not forget something. (eg. keys, files, errno,
  etc.)
- If the error reason is in beneath a variable string (such as `errno` or a caught exception), write `"Reason: %s", variable_as_string` at the end of the error
  message. Also end the preceding sentence with a dot. Example: `"XY failed. Reason: %s", exception.what()`.
- The last sentence (or single sentence of your message if you just provide one) must not end with a dot. This should encourage users to continue reading
  if other messages appear.
- If your message has multiple sentences, separate them with dots and start with a capital letter like you would do in the normal English language.
- Short but many sentences are preferable over long ones.
- Use abbreviations and acronyms with care. Not everybody will know them. You can write them in brackets though.
  (eg. `No Byte Order Mark (BOM) found` instead of `No BOM found`. But `XML does not support feature xy` is fine).
- Never use exclamation marks at the end of sentences.
- If it is unclear where embedded strings in error messages start and end ("%s"-strings), surround them by single quotes '.
  Sentences containing a variable string and end with `Reason: %s` do not need extra quotes because it is clear where they start and end.

# Examples

Actual reason might be in errno or an exception

- `"The configuration file %s contains invalid syntax at line %s. Reason: %s", file.path(), line, e.what()`
  Result: `The configuration file /etc/conf/file.csv contains invalid syntax at line 6. Reason: Missing column`
- `"Could not rename file %s. Reason: %s" file.path(), e.what()`
  Result: `Could not rename file /etc/conf/file.yml. Reason: File is not existent`
- `"The key %s contained the value '%s', but only 'unmounted' is supported for non-global clauses at the moment", keyName(key), keyString(key)`
  Result: `The key user:/my/key contained the value 'mounted', but only 'unmounted' is supported for non-global clauses at the moment`
