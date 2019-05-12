# CODING

This document provides an introduction in how the source code of
libelektra is organized and how and where to add functionality.

Make sure to read [DESIGN](/doc/DESIGN.md) together with this document.

## Folder structure

After you downloaded and unpacked Elektra you should see some folders.
The most important are:

- **src:** This directory contains the source of the libraries, tools and plugins.
- **doc:** General documentation for the project and the core library.
- **examples:** Examples on how to use the core library.
- **tests:** Contains the testing framework for the source (**src**).

## Source Code

libelektra is the ANSI/ISO C99-Core which coordinates the interactions
between the user and the plugins.

The plugins have all kinds of dependencies. It is the responsibility of
the plugins to find and check them using CMake. The same guidelines
apply to all code in the repository including the plugins.

`libloader` is responsible for loading the backend modules. It works on
various operating systems by using `libltdl`. This code is optimized
for static linking and win32.

kdb is the commandline-tool to access and initialize the Elektra database.

### General Guidelines

You are only allowed to break a guideline if there is a good reason
to do so. When you do, document the fact as comment next to the code.

Of course, all rules of good software engineering apply: Use meaningful
names and keep the software both testable and reusable.

The purpose of the guidelines is to have a consistent
style, not to teach programming.

If you see code that breaks guidelines do not hesitate to fix them. At least put a
TODO marker to make the places visible.

If you see inconsistency within the rules do not hesitate to talk about it with the
intent to add a new rule here.

See [DESIGN](/doc/DESIGN.md) document too, they complement each other.

### Code Comments

Code is not only for the computer, but it should be readable for humans, too.
Up-to-date code comments are essential to make code understandable for others.
Thus please use following techniques (in order of preference):

1. Comment functions with `/**/` and Doxygen, see below.

2. You should also add assertions to state what should be true at a specific
   position in the code. Their syntax is checked and they are automatically
   verified at run-time. So they are not only useful for people reading the
   code but also for tools. Assertions in Elektra are used by:

   `#include <kdbassert.h>`

   `ELEKTRA_ASSERT (condition, "formatted text to be printed when assert fails", ...)`

   Note: Do not use assert for user-APIs, always handle arguments of user-APIs like
   untrusted input.

3. If the "comment" might be useful to be printed during execution, use logging:

   `#include <kdblogger.h>`

   `ELEKTRA_LOG ("formatted text to be printed according to log filters", ...)`

   Read [HERE](/doc/dev/logging.md) for how to enable the logger.

4. Otherwise comment within source with `//` or with `/**/` for multi-line
   comments.

### Coding Style

- Limits

  - Functions should not exceed 100 lines.
  - Files should not exceed 1000 lines.
  - A line should not be longer than 140 characters.

  Split up when those limits are reached.
  Rationale: Readability with split windows.

- Indentation

  - Use tabs for indentation.
  - One tab equals 8 spaces.

- Blocks

  - Use blocks even for single line statements.
  - Curly braces go on a line on their own on the previous indentation level.
  - Avoid multiple variable declarations at one place.
  - Declare Variables as late as possible, preferable within blocks.

- Naming

  - Use camelCase for functions and variables.
  - Start types with upper-case, everything else with lower-case.
  - Prefix names with `elektra` for internal usage. External API either starts
    with `ks`, `key` or `kdb`.

- Whitespaces

  - Use space before and after equal when assigning a value.
  - Use space before round parenthesis ( `(` ).
  - Use space before and after `*` from Pointers.
  - Use space after `,` of every function argument.

The [reformat script](/scripts/reformat-source) can ensure most code style rules,
but it is obviously not capable of ensuring everything (e.g. naming conventions).
So do not give this responsibility out of hands entirely.

### C Guidelines

- The compiler shall not emit any warning (or error).
- Use goto only for error situations.
- Use `const` as much as possible.
- Use `static` methods if they should not be externally visible.
- C-Files have extension `.c`, Header files `.h`.
- Use internal functions: prefer to use elektraMalloc, elektraFree.

**Example:** [src/libs/elektra/kdb.c](/src/libs/elektra/kdb.c)

#### Clang Format

To guarantee consistent formatting we use [`clang-format`](https://clang.llvm.org/docs/ClangFormat.html) (version `6.0` or version `7.0`) to format all C and C++ code in the repository. Since our build servers also check the style for every pull request you might want to make sure you reformat your C/C++ code changes with this tool.

To find out which version of `clang-format` a certain build server uses please check:

- the [Debian sid Docker image](../scripts/docker/debian/sid/Dockerfile),
- the [Travis configuration file ](../.travis.yml), and
- the [Cirrus configuration file](../.cirrus.yml)

and search for the relevant packages (`clang-format`, `llvm`). Currently we use

- clang-format `6.0` in the [Debian sid image](../scripts/docker/debian/sid/Dockerfile) on the Jenkins build server,
- clang-format `7.0` in the [Travis configuration file ](../.travis.yml), and
- clang-format `7.0` in the [Cirrus macOS](../.cirrus.yml) build jobs

.

##### Installation

###### macOS

On macOS you can install `clang-format` using [Homebrew](https://brew.sh) either directly:

```sh
brew install clang-format
```

or by installing the whole [LLVM](http://llvm.org) infrastructure:

```sh
brew install llvm
```

. Please note, that both of these commands will install current versions of `clang-format` that might format code a little bit differently than Clang-Format `6.0` in certain edge cases. If you want you can also install Clang-Format `7.0` using LLVM `7.0`:

```
brew install llvm@7
```

.

###### Debian

In Debian the package for Clang-Format `6.0` is called `clang-format-6.0`:

```sh
apt-get install clang-format-6.0
```

.

##### Usage

For the basic use cases you can use `clang-format` directly. To do that, just call the tool using the option `-i` and specify the name of the files you want to reformat. For example, if you want to reformat the file `src/bindings/cpp/include/kdb.hpp` you can use the following command:

```sh
# On some systems such as Debian the `cmake-format` executable also contains
# the version number. For those systems, please replace `clang-format`,
# with `clang-format-6.0` in the command below.
clang-format -i src/bindings/cpp/include/kdb.hpp
```

. While this works fine, if you want to format only a small number of file, formatting multiple files can be quite tedious. For that purpose you can use the script [`reformat-source`](../scripts/reformat-source) that reformats all C and C++ code in Elektra’s code base

```sh
scripts/reformat-source # This script will probably take some seconds to execute
```

.

##### Tool Integration

If you work on Elektra’s code base regularly you might want to integrate the formatting step directly in your development setup. [ClangFormat’s homepage](https://clang.llvm.org/docs/ClangFormat.html) includes a list of integrations for various tools that should help you to do that. Even if this webpage does not list any integrations for your favorite editor or IDE, you can usually add support for external tools such as `clang-format` to advanced editors or IDEs pretty easily.

###### TextMate

While [TextMate](https://macromates.com) supports `clang-format` for the current file directly via the keyboard shortcut <kbd>ctrl</kbd>+<kbd>⇧</kbd>+<kbd>H</kbd>, the editor does not automatically reformat the file on save. To do that

1. open the bundle editor (“Bundles” → “Edit Bundles”),
2. navigate to the “Reformat Code” item of the C bundle (“C” → “Menu Actions” → “Reformat Code”),
3. insert `callback.document.will-save` into the field “Semantic Class”, and
4. change the menu option for the field “Save” to “Nothing”

. After that change TextMate will reformat C and C++ code with `clang-format` every time you save a file.

### C++ Guidelines

- Everything as in C if not noted otherwise.
- Do not use goto at all, use RAII instead.
- Do not use raw pointers, use smart pointers instead.
- C++-Files have extension `.cpp`, Header files `.hpp`.
- Do not use `static`, but anonymous namespaces.
- Write everything within namespaces and do not prefix names.
- Oriented towards [more safe and modern usage](https://github.com/isocpp/CppCoreGuidelines/blob/master/CppCoreGuidelines.md).

**Example:** [src/bindings/cpp/include/kdb.hpp](/src/bindings/cpp/include/kdb.hpp)

### CMake Guidelines

We use a similar style for CMake as we do for other code:

- The length of a functions should not exceed 100 lines.
- The length of a file should not exceed 1000 lines.
- A line should not be longer than 140 characters.
- Use tabs for indentation.
- One tab equals 8 spaces.
- Declare variables as late as possible, preferable within blocks.
- Add a space character before round parenthesis ( `(` ).
- Use lower case for command names (e.g. `set` instead of `SET`)

You can use [`cmake-format`](https://github.com/cheshirekow/cmake_format) to reformat code according to the guidelines given above. Since
`cmake-format` currently does not support tabs, please use the standard command `unexpand` to fix this issue. For example, to reformat the
file `CMakeLists.txt` in the root folder of the repository you can use the following command:

```sh
# This command uses `sponge`, which is part of the [moreutils](https://joeyh.name/code/moreutils/) package.
cmake-format CMakeLists.txt | unexpand | sponge CMakeLists.txt
```

. If you want to reformat the whole codebase you can use the script [`reformat-cmake`](/scripts/reformat-cmake).

### Java / Groovy Guidelines

Please follow
[Google Java Style Guide](https://google.github.io/styleguide/javaguide.html)
for Java and Groovy (used by Jenkins) files.

Most notably use:

- 2 spaces for indentation
- Variable and function names in lowerCamelCase
- K & R style brackets

### Markdown Guidelines

- File Ending is `.md` or integrated within Doxygen comments
- Only use `#` characters at the left side of headers/titles
- Use [fences](https://help.github.com/en/articles/creating-and-highlighting-code-blocks) for code/examples
- Prefer fences which indicate the used language for better syntax highlighting
- Fences with sh are for the [shell recorder syntax](/tests/shell/shell_recorder/tutorial_wrapper)
- `README.md` and tutorials should be written exclusively with shell recorder syntax
  so that we know that the code in the tutorial produces output as expected
- Please use [**title-case**](https://en.wiktionary.org/wiki/title_case) for headings in the general documentation.
- For [man pages](help/) please use **only capital letters for subheadings** and only **small letters for the main header**. We use this header style to match the look and feel of man pages for Unix tools such as `ls` or `mkdir`.

Please use [`prettier`](https://prettier.io) to format documentation according to the guidelines given above. If you want, you can also
format all Markdown files in the repository using the script [`reformat-markdown`](/scripts/reformat-markdown).

Under certain **exceptional** circumstances you might want to prevent `prettier` from formatting certain parts of a Markdown file. To do
that you can

- enclose the Markdown code in `<!-- prettier-ignore-start -->` and `<!-- prettier-ignore-end -->` tags, or
- use `<!-- prettier-ignore -->` to disable formatting till the end of a file

.

### Doxygen Guidelines

`doxygen` is used to document the API and to build the html and pdf output.
We also support the import of Markdown pages. Doxygen 1.8.8 or later
is required for this feature (Anyways you can find the
[API Doc](https://doc.libelektra.org/api/latest/html/) online).
Links between Markdown files will be converted with the
[Markdown Link Converter](/doc/markdownlinkconverter/README.md).
**Markdown pages are used in the pdf, therefore watch which characters you use and
provide a proper encoding!**

- use `@` to start Doxygen tags
- Do not duplicate information available in git in Doxygen comments.
- Use `@copydoc`, `@copybrief` and `@copydetails` intensively (except for file headers).

### File Headers

Files should start with:

```c
	/**
	 * @file
	 *
	 * @brief <short statement about the content of the file>
	 *
	 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
	 */
```

Note:

- `@` `file` has _no_ parameters.
- `@` `brief` should contain a short statement about the content of the file and is needed
  so that your file gets listed at https://doc.libelektra.org/api/latest/html/files.html

The duplication of the filename, author and date is not needed, because
this information is tracked using git and [doc/AUTHORS.md](AUTHORS.md) already.
