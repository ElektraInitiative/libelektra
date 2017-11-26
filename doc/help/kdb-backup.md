kdb-backup -- Make a backup of current KDB
==========================================

## SYNOPSES

`kdb backup`

## DESCRIPTION

This command will backup the `system`, `user` and `spec` configuration.
Afterwards a timestamp, which can be used to restore everything, will be printed.

The backup will be done in the `/var/tmp` directory, so make sure the backup is not deleted.

## EXAMPLES

```
kdb backup
#> restore with "kdb restore 1500000000"
```

## SEE ALSO
- [kdb-stash(7)](kdb-stash.md)
- [kdb-restore(7)](kdb-restore.md)
