# kdb-backup -- Make a backup of current KDB

## SYNOPSIS

`kdb backup`

## DESCRIPTION

This command will backup the `system`, `user` and `spec` configuration.
Afterwards a timestamp, which can be used to restore everything, will be printed.

The backup will be done in the `/var/tmp` directory, so make sure the backup is not deleted.

## EXAMPLES

```
kdb backup
#> kdb restore 1500000000
```

## SEE ALSO

- [kdb-stash(1)](kdb-stash.md)
- [kdb-restore(1)](kdb-restore.md)
