# kdb-cache(1) -- Enable, disable or clear the cache

## SYNOPSIS

`kdb cache {enable,disable,clear}`

## DESCRIPTION

This command is used to enable or disable the cache and to
clear the generated cache files in a safe way.

## LIMITATIONS

Caches are stored on a per-user basis, therefor the `clear`
subcommand can only remove a user's cache files (i.e. not system wide).

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile <profile>`:
  Use a different kdb profile.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-v`, `--verbose`:
  Explain what is happening. Prints additional information in case of errors/warnings.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.

## EXAMPLES

```sh
# Backup-and-Restore: system/elektra/cache

# Enable the cache
kdb cache enable

# Disable the cache
kdb cache disable

# Clear all generated cache files
kdb cache clear
```
