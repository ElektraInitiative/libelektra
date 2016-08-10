# CODING #

This document provides an introduction in how the source code of
libelektra is organized and how and where to add functionality.

Make sure to read [DESIGN](/doc/DESIGN.md) together with this document.

## Folder structure ##

After you downloaded and unpacked Elektra you see unusually many
folders. The reason is that the Elektra project consists of many activities.

The most important are:

 * **src:** This directory contains the source of the library and the tools.
 * **doc:** Documentation for the library
 * **example:** Examples on how to use the library
 * **tests:** contains the testing framework for the source.

## Source Code ##

libelektra is the ANSI/ISO C-Core which coordinates the interactions
between the user and the plugins.

The plugins have all kinds of dependencies. It is the responsibility of
the plugins to find and check them using CMake. The same guidelines
apply to all code in the repository including the plugins.

`libloader` is responsible for loading the backend modules. It works on
various operating systems by using `libltdl`. This code is optimized
for static linking and win32.

kdb is the commandline-tool to access and initialize the Elektra database.

### General Guidelines ###

You are only allowed to break a guideline if there is a good reason
to do so. When you do, document the fact, either in the commit message,
or as comment next to the code.

Of course, all rules of good software engineering apply: Use meaningful names and keep the software both testable and reusable.

The purpose of the guidelines is to have a consistent
style, not to teach programming.

If you see broken guidelines do not hesitate to fix them. At least put a
TODO marker to make the places visible.

If you see inconsistency do not hesitate to talk about it with the
intent to add a new rule here.

See [DESIGN](/doc/DESIGN.md) document too, they complement each other.


### Code Comments ###

Code is not only for the computer, but it should be readable for humans, too.
Up-to-date code comments are essential to make code understandable for others.

1. Ideally you should use assertions to state what should be true at a specific
   position in the code. Their syntax is checked and they are automatically
   verified at run-time. So they are not only useful for people reading the
   code.
   ```c
   #include <kdbassert.h>
   ELEKTRA_ASSERT (condition, "text to be printed when assert fails", ...)
   ```
2. If the "comment" might be useful to be printed during execution, use logging.
   First introduce your module `modulename` in `kdblog.h`.
   Then you can use it:
   ```c
   #define ELEKTRA_LOG_MODULE modulename
   #include <kdblog.h>
   ELEKTRA_LOG ("text to be printed according log filters", ...)
   ```
   Or if multiple modules are intermixed in the same file:
   ```c
   #include <kdblog.h>
   ELEKTRA_LOG (modulename, "text to be printed according log filters", ...)
   ```

3. Prefer to comment functions with Doxygen, see below.
4. Prefer to introduce variables and functions with good names that make the
   purpose clear.
5. Otherwise comment with `//`.





### Coding Style ###

- Limits

 * Functions should not exceed 100 lines.
 * Files should not exceed 1000 lines.
 * A line should not be longer than 140 characters.

Split up when those limits are reached.
Rationale: Readability with split windows.

- Indentation

 * Use tabs for indentation.
 * One tab equals 8 spaces.

- Blocks

 * Use blocks even for single line statements.
 * Curly braces go on a line on their own on the previous indentation level.
 * Avoid multiple variable declarations at one place.
 * Declare Variables as late as possible, preferable within blocks.

- Naming

 * Use camelCase for functions and variables.
 * Start types with upper-case, everything else with lower-case.
 * Prefix names with `elektra` for internal usage. External API either starts
with `ks`, `key` or `kdb`.

- Comments

 * Use C-comments `/**/` with doxygen style for functions.
 * Use C++-comments `//` for single line statements about the code in the
next line.

- Whitespaces

 * Use space before and after equal when assigning a value.
 * Use space before round parenthesis ( `(` ).
 * Use space before and after `*` from Pointers.
 * Use space after `,` of every function argument.

### C Guidelines ###

 * The compiler shall not emit any warning (or error).
 * Use goto only for error situations.
 * Use `const` as much as possible.
 * Use `static` methods if they should not be externally visible.
 * C-Files have extension `.c`, Header files `.h`.

**Example:** [src/libs/elektra/kdb.c](/src/libs/elektra/kdb.c)


### C++ Guidelines ###

 * Everything as in C if not noted otherwise.
 * Do not use goto at all, use RAII instead.
 * Do not use raw pointers, use smart pointers instead.
 * C++-Files have extension `.cpp`, Header files `.hpp`.
 * Do not use `static`, but anonymous namespaces.
 * Write everything within namespaces and do not prefix names.
 * Even though we use C++11, we should be oriented towards [more safe and modern usage](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md)

**Example:** [src/bindings/cpp/include/kdb.hpp](http://libelektra.org/tree/master/src/bindings/cpp/include/kdb.hpp)


### Doxygen Guidelines ###

`doxygen` is used to document the API and to build the html and pdf output.
We also support the import of Markdown pages. Doxygen 1.8.8 or later
is required for this feature (Anyways you can find the
[API Doc](http://doc.libelektra.org/api/latest/html/) online).
Links between Markdown files will be converted with the
[Markdown Link Converter](/doc/markdownlinkconverter/README.md).
**Markdown pages are used in the pdf, therefore watch which characters you use and
provide a proper encoding!**

 * use `@` to start Doxygen tags
 * Do not duplicate information available in git in Doxygen comments.
 * Use `@copydoc`, `@copybrief` and `@copydetails` intensively (except for file headers).

### File Headers ###

Files should start with:

\verbatim

	/**
	 * @file
	 *
	 * @brief <short statement about the content of the file>
	 *
	 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
	 */


\endverbatim

Note:

- `@` `file` has *no* parameters.
- `@` `brief` should contain a short statement about the content of the file and is needed
  so that your file gets listed at http://doc.libelektra.org/api/latest/html/files.html

The duplication of the filename, author and date is not needed, because
this information is tracked using git and doc/AUTHORS already.
