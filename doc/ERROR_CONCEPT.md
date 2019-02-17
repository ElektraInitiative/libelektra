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
