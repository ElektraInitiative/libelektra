# kdb-global-umount(1) - Unmount a global plugin from the key database

## SYNOPSIS

`kdb global-umount <name>`

## DESCRIPTION

Unmount a global plugin from the key database.

## OPTIONS

- `-H`, `--help`:
  Show the man page.
- `-V`, `--version`:
  Print version info.
- `-p`, `--profile <profile>`:
  Use a different kdb profile.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.

## EXAMPLES

```sh
if [ -f "$(kdb file system:/elektra/globalplugins)" ]; then mv "$(kdb file system:/elektra/globalplugins)" "globalplugins.bak"; else touch "globalplugins.rm"; fi

sudo kdb global-mount tracer

sudo kdb global-mount
# STDOUT-REGEX: .*tracer.*

sudo kdb global-umount tracer

sudo kdb global-mount
#>


if [ -f "globalplugins.rm" ]; then rm "$(kdb file system:/elektra/globalplugins)" "globalplugins.rm"; else mv "globalplugins.bak" "$(kdb file system:/elektra/globalplugins)"; fi
```

## SEE ALSO

- [kdb-global-mount(1)](kdb-global-mount.md).
- [elektra-mounting(7)](elektra-mounting.md).
- [elektra-plugins-framework(7)](/doc/dev/plugins-framework.md).
