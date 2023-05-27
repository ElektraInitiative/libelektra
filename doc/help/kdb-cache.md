# kdb-cache(1) -- Enable, disable or clear the cache

## SYNOPSIS

`kdb cache {enable,disable,default,clear}`

## DESCRIPTION

This command is used to enable or disable the cache and to revert
to the default settings. The default settings will let the system
decide whether to use the cache or not. The clear command will
remove the generated cache files in a safe way.

## LIMITATIONS

Caches are stored on a per-user basis, therefore the `clear`
subcommand can only remove a user's cache files (i.e. not system wide).

## OPTIONS

- `-H`, `--help`:
  Show usage of command.
- `-V`, `--version`:
  Print version info.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-v`, `--verbose`:
  Explain what is happening. Prints additional information in case of errors/warnings.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.

## EXAMPLES

```sh
# Backup-and-Restore: system:/elektra/cache

# Enable the cache
kdb cache enable

# Disable the cache
kdb cache disable

# Revert to defaults
kdb cache default

# Clear all generated cache files
kdb cache clear
```
