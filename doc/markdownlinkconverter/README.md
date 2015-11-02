# Markdown Link Converter

## Conventions

* Links starting with `@ref`, `#` for anchors and `http` for extern links
  wont be touched.
* Links to source code files wont be touched ether.
* To refer to a folder use `/` at the end of a link. This feature was introduced
  to be compatible with github, where you can show the content of a folder in
  combination with the README.md of the containing folder. Links ending with
  `/` will be changed to `/README.md`.
* Anchors wont work in imported markdown pages.

## About

The Markdown Link Converter (MLC) is used as input filter in doxygen, if a markdown
file is desired to be in the API documentation it only must have the extension
`md` and be in the `INPUT` path.

The MLC gives each markdown file a header `{ #header }` witch is attached to a title
and converts the links of other markdown files to refer to this header. This conversions
happens in 2 passes, witch is needed because there can be files with no title.


## Further improvements (which will be introduced in a later version):

* redirect links to code files right
* absolute targets in links
