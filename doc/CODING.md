# CODING #

This document provides an introduction in how the source code of
libelektra is organized and how and where to add functionality.

Make sure to read [DESIGN](DESIGN.md) together with this document.

## Folder structure ##

After you downloaded and unpacked Elektra you see untypically many
folders. The reason is that Elektra project consists of many activities.

The most important are:

 * **src:** Here is the source for the library and the tools itself.
 * **doc:** Documentation for the library.
 * **example:** Examples of using the library.
 * **tests:** Is the testing framework for the source.

## Source Code ##

libelektra is the ANSI/ISO C-Core which does interacts between the user
and the plugins.

The plugins have all kinds of dependencies. It is the responsibility of
the plugins to find and check them using CMake. The same guidelines
apply for all code in the repository including the plugins.

`libloader` is responsible for loading the backend modules. It works for
various operating systems by using `libltdl`, but has an optimized code
for static linking and win32.

kdb is the commandline-tool to access and initialize the Elektra database.

### General Guidelines ###

It is only allowed to break a guideline if there is a good reason
for it. When doing so, document the fact either in the commit message,
or as comment next to the code.

Of course all rules of good software engineering apply, like to
use good names, make software testable and reusable.
The purpose of the guidelines is to have a consistent
style, not to teach programming.

If you see broken guidelines do not hesitate to fix them. At least put a
TODO marker to make the places visible.

If you see inconsistency do not hesitate to talk about it with the
intent to add a new rule here.

See [DESIGN](DESIGN.md) document too, they complement each other.


### C Guidelines ###

 * Functions should not exceed 100 lines.
 * Files should not exceed 1000 lines.
 * A line should not be longer then 72 characters.
 * Split up when those limits are reached.
 * Rationale: Readability with split windows.
 * The compiler shall not emit any warning (or error).
 * Use tabs for indentation.
 * Prefer to use blocks to single line statements.
 * Curly braces go on a line on their own on the previous indentation level
 * Use goto only for error situations.
 * Use camelCase for functions and variables.
 * Start types with upper-case, everything else with lower-case.
 * Avoid spaces between words and round braces.
 * Use C-comments `/**/` with doxygen style for functions.
 * Use C++-comments `//` for single line statements about the code in the
next line.
 * Avoid multiple variable declarations at one place and `*` are next to the
variable names.
 * Use `const` as much as possible.
 * Use `static` methods if they should not be externally visible.
 * Declare Variables as late as possible, preferable within blocks.
 * C-Files have extension `.c`, Header files `.h`.
 * Prefix names with `elektra` for internal usage. External API either starts
with `ks`, `key` or `kdb`.

**Example:** [src/libelektra/kdb.c](../src/libelektra/kdb.c)


### C++ Guidelines ###

 * Everything as in C if not noted otherwise.
 * Do not use goto at all, use RAII instead.
 * Do not use raw pointers, use smart pointers instead.
 * C++-Files have extension `.cpp`, Header files `.hpp`.
 * Do not use `static`, but anonymous namespaces.
 * Write everything within namespaces and do not prefix names.

**Example:** [src/bindings/cpp/include/kdb.hpp](../src/bindings/cpp/include/kdb.hpp)


### Doxygen Guidelines ###

`doxygen` is used to document the API and to build the html and pdf output.
We support also the import of markdown pages, but a minimum version of 1.8.8
of Doxygen is required for this feature (Anyways you can find the
[API Doc](http://doc.libelektra.org/api/latest/html/) online).
Links between markdown files will be converted with the
[Markdown Link Converter](markdownlinkconverter/README.md).
**Markdown pages are used in the pdf, therefore watch your characters and
provide a proper encoding!**

Do not duplicate information available in git in doxygen.
Use `\copydoc`, `\copybrief` and `\copydetails` intensively.


Files should start with:

\verbatim

	/**
	* @file
	*
	* @brief <short statement about the content of the file>
	*
	* @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
	*
	*/

\endverbatim

Note:
- file has *no* parameters.
- brief should contain a short statement about the content of the file.

The duplication of the filename, author and date is not needed, because
this information is tracked using git.
