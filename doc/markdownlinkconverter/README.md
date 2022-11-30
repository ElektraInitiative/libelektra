# Markdown Link Converter

The Markdown link Converter, which filters Markdown pages before the processing
of doxygen, converts the links in Markdown pages. It is set up as input filter
in doxygen, if a Markdown file is desired to be in the API documentation
it only must have the extension `.md` and be in the `INPUT` path.

The Markdown link Converter gives each Markdown file a header `{ #header }` which is attached to a title
and converts the links to refer to this headers. This conversion
happens in 2 passes, which is needed because there can be files with no title.

## Usage for Manual Invocation

```sh
markdownlinkconverter [<cmake-cache-file>] <input-file>
```

**The <input-file> parameter must be an absolute path!**

## Conventions

- Links starting with `@ref`, `#` for anchors and `http`, `https` or `ftp` for extern links
  won't be touched.
- All other links to Markdown or arbitrary source files will be converted.
- All links to folders will be altered to the README.md in the Folder.
  This feature was introduced to be compatible with GitHub, where you can show the content of a folder in
  combination with the README.md of the containing folder.
- Anchors won't work in imported Markdown pages.

## GitHub Specialities

- GitHub supports source code fences with syntax highlighting which are not recognized by Doxygen.
  Thus `sh` after the fence is removed for Doxygen.

## Link Validation

### Internal Links

The link validation works with a simple try to `fopen` the file,
which the link refers to.

### External Links

Every link starting with `http`, `https` or `ftp` will be written to a file named `external-links.txt` located in your
build folder. With the following syntax:

```
<file>:<line>:0 <url>
```

Note: Due to the nature of the Markdown Link Converter the file can only be opened in append mode. So delete it and rerun the
html build process (`make clean` could be needed) to get a list without duplicates.

In the script folder is a script named `link-checker`. This script can be used to validate the links.
Broken links will be printed. False positive not excluded (very rare).

This link-checker reads the links from an input file (the synopsis is `link-checker <file>`) and prints the broken ones to the `stderr`.
`wget` is used for the check so this program is required to be installed on your system.

The link-checker includes the ability to list links on a [whitelist](../../tests/linkchecker.whitelist) which will not be checked and so on not printed to `stderr` in any case.
Furthermore, this is the only way to let `http` links pass as they should be prevented by default.
However, this does not mean that the `http` links are reachable as due to the lack of encryption, availability tests are not sensible in this case.

## Further Improvements (Which Will be Introduced in a Later Version):

- optimize pdf output (also UTF-8 encoding)
- if title contains --, this should be @brief
- also remove other fences doxygen does not understand
