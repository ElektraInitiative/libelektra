# Doxygen

[Doxygen](https://www.doxygen.nl/) is used to generate documentation of the API and Markdown files.
The output formats can be:

- HTML sites (online documentation)
- Latex files (offline documentation)
- RTFs (MS-Word)
- PostScript files
- hyperlinked PDFs
- Unix man pages

When referencing Markdown files, the links between them will be resolved with the
[Markdown Link Converter](/doc/markdownlinkconverter/README.md).

## Usage

To invoke the generation with Doxygen, the so-called Doxyfile is needed:

```sh
doxygen Doxyfile
```

Elektra's Doxyfile is located at `<PROJECT_ROOT>/doc/Doxyfile`.
This file is used for the configuration of the whole documentation generation process.
The documentation for the Doxyfile can either be found within the file or at the
[Doxygen Website](https://www.doxygen.nl/manual/config.html).

## Mermaid JS

[Mermaid JS](https://mermaid.js.org/) is used for generating diagrams within the API.
The graphs and diagrams are stored as `.mmd` files and are located at `<PROJECT_ROOT>/doc/images/mermaid`.

Mermaid files can be included using the configured alias in the Doxyfile:

```doxygen
ALIASES += mermaid{1}="@htmlonly <div class=\"mermaid\"> ^^ @endhtmlonly @htmlinclude \"\1.mmd\" @htmlonly ^^ </div> @endhtmlonly"
```

The alias shows that the new command `mermaid` needs one parameter. This parameter is the name of the
Mermaid file **without** the file extension.

The command can be used inside the API documentation as follows:

```doxygen
 /**
  * @mermaid{FILENAME_WITHOUT_EXTENSION}
  */
```

In Markdown files there is currently no support for including Mermaid files within them easily.
One solution is to write Mermaid code within Markdown:

````Markdown
```mermaid
  graph TD;
      A-->B;
      A-->C;
      B-->D;
      C-->D;
```
````

The other solution would be to convert the Mermaid files to SVGs and including them automatically.
For this task, [Mermaid CLI](https://github.com/mermaid-js/mermaid-cli) could be used.

**NOTE**: Mermaid CLI is currently not integrated in Elektra.

For further information on Mermaid JS and Doxygen, [this tutorial](https://github.com/tttapa/doxygen-mermaid)
had been used to include Mermaid JS.

## Working Locally

When working with Doxygen locally, some obstacles have to be overcome.

First of all, CMake variables are used all over the Doxyfile:

```doxygen
...
PROJECT_LOGO = @PROJECT_SOURCE_DIR@/doc/images/logo/logo_color_doxygen.svg
...
```

`@PROJECT_SOURCE_DIR@` is such a variable, they can be detected by the leading and ending @ sign.
CMake replaces those variables with meaningful values within the make process of the project.

As developers do not tend to execute the make process each time before testing some changes in the Doxyfile,
it is recommended to maintain a copy of the Doxyfile, a local version.

As the main goal is that Doxygen generates useful output, the local version needs to be adapted.
This can either be done by searching and replacing variables in the local Doxyfile or using the Doxywizard:

```sh
doxywizard Doxyfile-local
```

The [Doxywizard](https://www.doxygen.nl/manual/doxywizard_usage.html) is a GUI that helps to set values in the Doxyfile.
The benefit of using the GUI is that each possible command is explained and invalid values for commands are highlighted
red. This helps to find those values that lead to exceptions.

---

**NOTE**: The local Doxyfile should never be integrated in the version control system. Each change made to the local
Doxyfile needs to be integrated to the actual Doxyfile by **using the CMake variables** of the project.

---
