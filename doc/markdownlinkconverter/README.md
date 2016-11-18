# Markdown Link Converter

The Markdown link Converter, which filters markdown pages before the processing
of doxygen, converts the links in markdown pages. It is set up as input filter
in doxygen, if a markdown file is desired to be in the API documentation
it only must have the extension `.md` and be in the `INPUT` path.

The Markdown link Converter gives each markdown file a header `{ #header }` which is attached to a title
and converts the links to refer to this headers. This conversion
happens in 2 passes, which is needed because there can be files with no title.

## Usage for manual invocation

	markdownlinkconverter [<cmake-cache-file>] <input-file>

**The <input-file> parameter must be an absolute path!**

## Conventions

* Links starting with `@ref`, `#` for anchors and `http`, `https` or `ftp` for extern links
  wont be touched.
* All other links to markdown or arbitrary source files will be converted.
* All links to folders will be altered to the README.md in the Folder.
  This feature was introduced to be compatible with github, where you can show the content of a folder in
  combination with the README.md of the containing folder.
* Anchors wont work in imported markdown pages.

## Link Validation

### internal links

The link validation works with a simple try to `fopen` the file,
which the link refers to.

### external links

Every link starting with `http`, `https` or `ftp` will be written to a file named `external-links.txt` located in your
build folder. With the following syntax:

	<file>|<line> col 0 | <url>

Note: Due to the nature of the Markdown Link Converter the file can only be opened in append mode. So delete it and rerun the
html build process (`make clean` could be needed) to get a list without duplicates.

In the script folder is a script named `link-checker`. This script can be used to validate the links.
Broken links will be printed. False positive not excluded (very rare).

## Further improvements (which will be introduced in a later version):

* optimize pdf output (also UTF-8 encoding)
* if title contains --, this should be @brief
