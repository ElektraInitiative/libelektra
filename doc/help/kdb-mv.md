kdb-mv(1) -- Move keys within the key database
==============================================

## SYNOPSIS

`kdb mv <source> <dest>`  

Where `source` is the path of the key(s) you want to copy and `dest` is the path where you would like to move the key(s) to.
Note that when using the `-r` flag, `source` as well as all of the keys below it will be moved.

## DESCRIPTION

This command moves key(s) in the Key database.  
You can move keys to another directory within the database or even below another key.  


## OPTIONS

- `-H`, `--help`:
  Print help text.
- `-V`, `--version`:
  Print version info.
- `-r`, `--recursive`:
  Work in a recursive mode.
- `-v`, `--verbose`:
  Explain what is happening.

## EXAMPLES

To move a multiple keys:  
	`kdb mv -r user/example1 user/example2`  

To move a single key:  
	`kdb mv user/example/key1 user/example/key2`  


