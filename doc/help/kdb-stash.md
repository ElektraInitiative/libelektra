# kdb-stash -- Stash away KDB to be restored later

## SYNOPSIS

`kdb stash`

## DESCRIPTION

This command will stash away the `system`, `user` and `spec` configuration, i.e. it releases all mount points and reset Elektra to a clean state.
Afterwards a timestamp which can be used to restore everything will be printed.

The backup will be done in the `/var/tmp` directory, so make sure the backup is not deleted.

## EXAMPLES

```
kdb set user:/x foo
#>
#> Create a new key user:/x with string "foo"

kdb mount a.ini /a
kdb mount
#> a.ini on /a with name /a
#> none on system:/info/elektra/constants with name system:/info/elektra/constants
#> /usr/local/share/doc/elektra/CONTRACT.ini on system:/info/elektra/contract/#0 with name system:/info/elektra/contract/#0
#> none on system:/info/elektra/desktop with name system:/info/elektra/desktop
#> /usr/local/share/doc/elektra/METADATA.ini on system:/info/elektra/metadata/#0 with name system:/info/elektra/metadata/#0
#> none on system:/info/elektra/uname with name system:/info/elektra/uname

kdb backup
#> kdb restore 1500000000

kdb get user:/x
#> Did not find key

kdb mount

kdb restore 1500000000

kdb get user:/x
#> foo

kdb mount
#> a.ini on /a with name /a
#> none on system:/info/elektra/constants with name system:/info/elektra/constants
#> /usr/local/share/doc/elektra/CONTRACT.ini on system:/info/elektra/contract/#0 with name system:/info/elektra/contract/#0
#> none on system:/info/elektra/desktop with name system:/info/elektra/desktop
#> /usr/local/share/doc/elektra/METADATA.ini on system:/info/elektra/metadata/#0 with name system:/info/elektra/metadata/#0
#> none on system:/info/elektra/uname with name system:/info/elektra/uname
```

## SEE ALSO

- [kdb-restore(1)](kdb-restore.md)
- [kdb-backup(1)](kdb-backup.md)
