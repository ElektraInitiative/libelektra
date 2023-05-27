# kdb-gen(1) -- Elektra's code-generator

## SYNOPSIS

`kdb gen <templateName> <parentKey> <outputName> [parameters...]`

- `<templateName>`:
  one of the templates listed [below](#templates)
- `<parentKey>`:
  the parent key to use, templates may have certain restrictions on e.g. the allowed namespaces
  You may also use the special value `-` (see below).
- `<outputName>`:
  the base name of the output files. If a template produces multiple files, it will append different
  suffixes (e.g. file extensions) to this base name.
- `[parameters...]`:
  a list of parameters, the supported parameters depend on the template

## DESCRIPTION

This command invokes Elektra's code-generator.

It supports different templates. All templates require a `parentKey` parameter, because this determines
the input for the code-generator, as well as an `outputName` parameter to specify the output file(s).

If the given `parentKey` is `-`, instead of actually invoking the code generator, `kdb gen` will just
print the filenames of the files that would be written with a valid `parentKey`.

For more information see the [list of templates](#templates) below and the man-pages for each of them.

## RETURN VALUES

This command will return the following values as an exit status:

- 0:
  No errors.
- 1-10:
  standard exit codes, see [kdb(1)](kdb.md)

## OPTIONS

- `-H`, `--help`:
  Show usage of command.
- `-V`, `--version`:
  Print version info.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-F`, `--input-file <plugin>=<file>`:
  Load the file `<file>` with plugin `<plugin>` instead of accessing the KDB.
- `-v`, `--verbose`:
  Explain what is happening.
  Gives a complete trace of all tried keys.
  Very useful to debug fallback and overrides.
- `-d`, `--debug`:
  Give debug information. Prints additional debug information in case of errors/warnings.

## TEMPLATES

Currently we only have one template:

- `highlevel`:
  Generates the files needed for using the high-level API with code-generation. More information
  can be found in [kdb-gen-highlevel(1)](kdb-gen-highlevel.md)

## EXAMPLES

Examples can be found on the man-pages of each template.

## SEE ALSO

- [kdb(1)](kdb.md) for general information about the `kdb` tool.
- [elektra-key-names(7)](elektra-key-names.md) for an explanation of key names.
