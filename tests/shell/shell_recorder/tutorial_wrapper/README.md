# Markdown to Shell Recorder #

## Syntax ##

* Checks

  `# CHECK-OPTION:VALUE`

  * `# RET:N`

     compares the return code of the command to N. if not specified, 0 is implied.

  * `# ERROR:N`

     checks if the error with the code N was thrown

  * `# WARNINGS:REGEX-STRING`

     `REGEX-STRING` is matched against a comma-separated list of the warnings thrown by the command using posix-extended regex

  * `# STDOUT:STRING`

     `STRING` is matched 1:1 against the command output. newlines must be encoded as `⏎` 

  * `#> STRING`

     `STRING` is matched 1:1 against the command output. multiple `#> ` will be concatenated automatically using `⏎`

  * `# STDOUT-REGEX:REGEX-STRING`

     `REGEX-STRING` is matched agains the output of the command using posix-extended regex. newlines must be encoded as `⏎`

* Comments

  all lines starting with `#` that aren't checks are treated as comments and will be ignored

* Empty lines

  will be ignored

* Commands

  lines not starting with a `#` are treated as (shell-)commands and executed by the shell recorder.
  `sudo` commands will be executed without `sudo`
  for multiline commands each line except the last one must end with a `\`
