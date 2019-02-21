# Error Message Concept

The purpose of this file is solely to better comment on in a Pull Request and gather feedback.

## Findings/ Motivation

1. Too many errors in the [specification](src\error\specification) (210 errors/warnings)
2. Many duplicate or redundant errors
3. Description/ Reason are very similar
4. Potential file splitting (1327 LOC)

## Some facts

1. Some errors have the same text but are duplicated because one is `warning` and the other one is `error`/`fatal` 
(#153 + #154, #128 + #129, #120 + #41, #44 + 45 , #66 + #67, etc.)
2. Some errors might be duplicated just because they belong to different plugins (or the dev neglected to lookup a similar in the specification) 
(#29 + #57 + #72 + #169, #10 + #61, etc.)
3. Many errors are very similar or could be summarized and are differentiated via the reason field 
(#54 + #55, #31 - #33, #93 + #7 + #17, #74 + #75, #77 + #10 + #61, etc.)
4. Some errors are marked as unused (#22 + #48 + #58 + #66 + #67 + #68)

## Evaluation

From now on I will take the following error message as example:

```
1. The command kdb set failed while accessing the key database with the info:
2. Sorry, the error (#121) occurred ;(
3. Description: validation failed
4. Reason: Validation of key "user/sw/lcdproc/lcdd/#0/current/curses/background" with string "greeeeen" failed.
5. Ingroup: plugin
6. Module: enum
7. At: ....../src/plugins/enum/enum.c:218
8. Mountpoint: user/sw/lcdproc/lcdd/#0/current
9. Configfile: ...../LCDd.conf.25676:1549919217.284067.tmp
```

Generally I would suggest that this error message is way too much information for a user. We can agree on that #4 is by far
the most relevant line.

The lines are numbered and will be referred in the following sections.

### Line 9: Configfile

Reasons to keep it:
* If users want to see where the concrete file is located (basically if it is the file itself or some overwritten file)

Reasons against it:
* Very verbose line
* I highly doubt that most common users will require this information (unneccesary)

Suggestions:
Remove it completely. If users want to know the location they should use `kdb file` on the concrete setting.

### Line 8: Mountpoint

Reasons to keep it:
* If users want to see the parent key of their configuration
* If I remember correctly: all error information is saved to this key, so users could investigate further errorinformation which is saved
to this key (in the metadata)

Reasons against it:
* Another extra line in the error message.
* I highly doubt that most common users will require this information (unneccesary)

Suggestions:
//TODO: Validate this
Remove it completely. If users want to know the location they should use `kdb file` on the concrete setting.

### Line 7: At

Reasons to keep it:
* Debug purposes

Reasons against it:
* Another extra line in the error message.
* I highly doubt that most common users will require this information (unneccesary)

Suggestions:
Remove it for standard error messages. Maybe a `kdb set --verbose` could include such information for developers, users.
Alternatively, another possibility would be to have a command which reveals the last error message with more detail: 
`kdb last-message`.

### Line 6: Module
...

### Error/Warning/Fatal etc. splitting in the specification file

I would suggest to remove this concept from the specification and let the developer use a respective method to indicate if it is a warning/error/etc.
The macros ELEKTRA_SET_ERROR, ELEKTRA_ADD_WARNING ... should be enough to make developers cautious of wrong method usage.
What I have seen is that the return code is the only matter if it is an error or not.


