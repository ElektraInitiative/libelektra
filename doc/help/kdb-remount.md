# kdb-remount(1) - Use an existing backend to mount a new file

## SYNOPSIS

`kdb remount <new path> <new mount point> <existing mount point>`

Where `new path` is the path to the file the user wants to mount, (Absolute for system files, relative for user files)<br>
`new mount point` is where in the key database the new configuration file should be mounted at, (For a cascading mount pount, `mountpoint` should start with `/`)<br>
and `existing mount point` is the mount point where the existing backend is mounted.<br>

## DESCRIPTION

This command allows a user to use an existing backend (such as one from a previous mount) to mount a new configuration file to a new mount point in the key database.<br>

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
  Give debug information or ask debug questions (in interactive mode). Prints additional debug information in case of errors/warnings.
- `-i`, `--interactive`:
  Instead of passing all information by parameters
  ask the user interactively.

## EXAMPLES

To mount a new file using an existing backend that is mounted to `system:/example`:<br>
`kdb remount /etc/configuration2.ini system:/example2 system:/example`<br>
