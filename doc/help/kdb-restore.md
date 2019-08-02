# kdb-restore -- Restore from backup

## SYNOPSES

`kdb restore <timestamp>`

## DESCRIPTION

This command will restore everything backed up by `kdb backup` (or stashed by `kdb stash`)
where `timestamp` is the timestamp returned by `kdb backup` (or `kdb stash`).

## EXAMPLES

```
kdb restore 1500000000
```

## SEE ALSO

- [kdb-backup(1)](kdb-backup.md)
- [kdb-stash(1)](kdb-stash.md)
