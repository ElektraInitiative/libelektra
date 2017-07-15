kdb-backup -- Backup and unmount everything
===========================================

## SYNOPSES

`kdb backup`

## DESCRIPTION

This command will backup the `system`, `user` and `spec` configuration, release all mountpoints and reset Elektra to a clean state. Afterwards a timestamp which can be used to restore everything will be printed.

## EXMAPLES

```
kdb set user/x foo
#>
#> Create a new key user/x with string foo

kdb mount a.ini /a
kdb mount
#> a.ini on /a with name /a
#> none on system/info/elektra/constants with name system/info/elektra/constants
#> /usr/local/share/doc/elektra/CONTRACT.ini on system/info/elektra/contract/#0 with name system/info/elektra/contract/#0
#> none on system/info/elektra/desktop with name system/info/elektra/desktop
#> /usr/local/share/doc/elektra/METADATA.ini on system/info/elektra/metadata/#0 with name system/info/elektra/metadata/#0
#> none on system/info/elektra/uname with name system/info/elektra/uname

kdb backup
#> restore with "kdb restore 1500000000"

kdb get user/x
#> Did not find key

kdb mount

kdb restore 1500000000

kdb get user/x
#> foo

kdb mount
#> a.ini on /a with name /a
#> none on system/info/elektra/constants with name system/info/elektra/constants
#> /usr/local/share/doc/elektra/CONTRACT.ini on system/info/elektra/contract/#0 with name system/info/elektra/contract/#0
#> none on system/info/elektra/desktop with name system/info/elektra/desktop
#> /usr/local/share/doc/elektra/METADATA.ini on system/info/elektra/metadata/#0 with name system/info/elektra/metadata/#0
#> none on system/info/elektra/uname with name system/info/elektra/uname
```

## SEE ALSO
- [kdb-restore(7)](kdb-restore.md)
