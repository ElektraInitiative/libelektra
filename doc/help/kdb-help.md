# kdb-help(1) -- Show man page for elektra tools

## SYNOPSIS

```sh
kdb help <tool>
kdb --help <tool>
kdb -H <tool>
```

## DESCRIPTION

Show a man page for one of Elektra’s tools.
Note that `kdb <tool> --help` might have a different behavior, depending on the tool.

Also note that, no additional commandline options are allowed for this kind of
invocation. If you want that use `--help` or `-H` after `<tool>`.

## KDB

- `/sw/elektra/kdb/#0/current/man`:
  The man(1) utility to be used.
  Defaults to /usr/bin/man

## EXAMPLES

Show how to set keys:
`kdb help set`

Use the info program as man page viewer for the current user:
`kdb set user:/sw/elektra/kdb/#0/current/man /usr/bin/info`
