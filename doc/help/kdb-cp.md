kdb-cp(1) -- Copy keys within the key database
==============================================

## SYNOPSIS

`kdb cp <source> <dest>`


## DESCRIPTION

This command copies key(s) in the Key database.
You can copy keys to another directory within the database or even below another key.
Note that you can't copy a key below itself.

Where `source` is the path of the key(s) you want to copy and `dest` is the path where you would like to copy the key(s) to.
Note that when using the `-r` flag, `source` as well as all of the keys below it will be copied.

## OPTIONS


- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile`=<profile>:
  Use a different kdb profile.
- `-r`, `--recursive`:
  Recursively copy keys.
- `-v`, `--verbose`:
  Explain what is happening.
- `-C`, `--nocolor`:
  Disable colored output.



## EXAMPLES

To copy multiple keys:
`kdb cp -r user/example1 user/example2`

To copy a single key:
`kdb cp user/example/key1 user/example/key2`

To copy keys below an existing key:
`kdb cp -r user/example user/example/key1`
Note that in this example, all keys in the example directory will be copied below `key1` EXCEPT `key1`.



