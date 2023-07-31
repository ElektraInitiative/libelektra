# kdb-convert(1) -- Convert configuration files using elektra

## SYNOPSIS

`kdb convert [<import-format>] [<export-format>] [<import-file>] [export-file]`

## DESCRIPTION

This command allows a user to convert a file from any format supported by Elektra to any other supported format.

This command relies on the functionality of Elektra but does not actually modify the key database in any way.

This command uses plugins to specify and convert between formats, it is only limited by the plugins available for Elektra.

## USAGE

Where `import-format` is the format that the current configuration file is using, `export-format` is the format the user wishes to convert it to, `import-file` is the full path to the current configuration file, and `export-file` is where the converted configuration file should be saved.

If either `import-format` or `export-format` is not specified, the `storage` plugin will be used instead.
The `storage` plugin can be configured at compile-time or changed by the link `libelektra-plugin-storage.so`.
If either `import-file` or `export-file` are not specified, `stdin` and `stdout` are used respectively.

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

To convert an Elektra dump file to xml:<br>
`cat sw.ecf | kdb convert dump xmltool > sw.xml`

Another way to convert an Elektra dump file to xml:<br>
`kdb convert dump xmltool /home/user/dump_file.ecf /home/user/xml_file.xml`

To print an xml file using the `line` format:<br>
`cat ../tests/xml_file.xml | kdb convert xmltool line`<br>
Note that this command won't save the output, it will just display it to `stdout`.
