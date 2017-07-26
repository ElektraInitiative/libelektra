kdb-global-umount(1) - Unmount a global plugin from the key database
====================================================

## SYNOPSIS

`kdb global-umount <name>`

## DESCRIPTION

Unmount a global plugin from the key database.

## EXAMPLES

```sh

sudo kdb global-mount tracer

sudo kdb global-mount
# STDOUT-REGEX: .*tracer\nspec.*

sudo kdb global-umount tracer

# spec is always mounted by default
sudo kdb global-mount
#> spec

sudo kdb global-umount spec

sudo kdb global-mount
#> 
```

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile`=<profile>:
  Use a different kdb profile.
- `-C`, `--color`=[when]:
  Print never/auto(default)/always colored output.

## SEE ALSO

- [kdb-global-mount(7)](kdb-global-mount.md).
- [elektra-mounting(7)](elektra-mounting.md).
- [elektra-plugins-framework(7)](elektra-plugins-framework.md).
