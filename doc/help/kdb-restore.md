# kdb-restore -- Restore from backup

## SYNOPSIS

`kdb restore <timestamp>`

## DESCRIPTION

This command will restore everything backed up by `kdb backup` (or stashed by `kdb stash`)
where `timestamp` is the timestamp returned by `kdb backup` (or `kdb stash`).

## WARNING

These changes cannot be undone, please use with care!
This command writes into the `/etc` directory and as such it requires root permissions.

## EXAMPLES

```sh
kdb set user:/tests/x foo
#>
#> Create a new key user:/tests/x with string "foo"

sudo kdb mount a.ini /a
sudo kdb mount
#> a.ini on /a with name /a
#> none on system:/info/elektra/constants with name system:/info/elektra/constants
#> /usr/local/share/doc/elektra/CONTRACT.ini on system:/info/elektra/contract/#0 with name system:/info/elektra/contract/#0
#> none on system:/info/elektra/desktop with name system:/info/elektra/desktop
#> /usr/local/share/doc/elektra/METADATA.ini on system:/info/elektra/metadata/#0 with name system:/info/elektra/metadata/#0
#> none on system:/info/elektra/uname with name system:/info/elektra/uname

sudo kdb backup
#> kdb restore 1500000000

kdb get user:/tests/x
#> Did not find key

kdb mount

kdb restore 1500000000

kdb get user:/tests/x
#> foo

kdb mount
#> a.ini on /a with name /a
#> none on system:/info/elektra/constants with name system:/info/elektra/constants
#> /usr/local/share/doc/elektra/CONTRACT.ini on system:/info/elektra/contract/#0 with name system:/info/elektra/contract/#0
#> none on system:/info/elektra/desktop with name system:/info/elektra/desktop
#> /usr/local/share/doc/elektra/METADATA.ini on system:/info/elektra/metadata/#0 with name system:/info/elektra/metadata/#0
#> none on system:/info/elektra/uname with name system:/info/elektra/uname

sudo kdb umount /a
```

## SEE ALSO

- [kdb-backup(1)](kdb-backup.md)
- [kdb-stash(1)](kdb-stash.md)
