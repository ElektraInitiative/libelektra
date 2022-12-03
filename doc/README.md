# Documentation Index

This folder contains documentation for “Elektra – the configuration framework for everyone”. If you do not know what Elektra is, we recommend that you check out our [homepage](https://www.libelektra.org/home) first. This ReadMe deals with the content of the documentation folder and should give you a hint where to look for specific information.

## Introductory

- [Goals](GOALS.md): We specify the goals and target audiences for Elektra in this document.
- [Why](WHY.md): This document describes why you should use Elektra.
- [Vision](VISION.md): This document describes the vision behind Elektra.
- [Big Picture](BIGPICTURE.md): This document provides an birds eye view of Elektra and the key database (KDB).
- [Security](SECURITY.md): This guideline shows how Elektra handles security concerns.
- [Tutorials](tutorials): The tutorials folder provides various **user related tutorials**. If you are interested in **developer related tutorials** instead, then please take a look at the folder [dev](dev).
- [News](news): The news folder contains release notes and other recent information about Elektra.
- [Paper](paper): This directory contains a research paper about Elektra, also available in [PDF](http://joss.theoj.org/papers/10.21105/joss.00044) format.

## Using Elektra

- [Installation](INSTALL.md): These instructions tell you how you can install Elektra in your favorite operating system.
- [Compile](COMPILE.md): If you want to compile Elektra from source, please look at this document.
- [Debugging](DEBUGGING.md): If you want to debug Elektra, please look at this document.
- [Help](help): This folder contains our man pages in Markdown format. The folder [man](man) contains these man pages in roff format, which you can read using the Unix utility [`man`](https://en.wikipedia.org/wiki/Man_page) if you already installed Elektra.
- [Keynames](KEYNAMES.md): This document describes how Elektra's keynames work.

### API

- [API](API.md): This overview of the application programming interface tells you how you can develop an application that uses Elektra.
- [Design](DESIGN.md): This document describes the design of Elektra’s C API.

## Advanced Information

- [Metadata](METADATA.ini): This document specifies data about the KDB (meta information), like supported data types and configuration options.
- [Contract](CONTRACT.ini): The plugin contract specifies keys and values that an [Elektra plugin](../src/plugins) provides.

## Elektra Internals

- [Copy-on-write](contrib/copy_on_write.md): This document gives a technical overview over our copy-on-write implementation for `Key` and `KeySet`.

## Contributing

- [Coding](CODING.md): The coding guidelines describe the basic rules you should keep in mind when you want to contribute code to Elektra.
- [Git](GIT.md): This document describes how we use the version control system [git](https://git-scm.com) to develop Elektra.
- [Ideas](IDEAS.md): If you want to contribute to Elektra and do not know what, you can either take a look here or at our [issue tracker](http://libelektra.org/issues).
- [ToDo](todo): This folder contains various ToDo items for future releases of Elektra.
- [Authors](AUTHORS.md): This file lists information about Elektra’s authors.

## Other

- [Images](images): The images folder contains logos and other promotional material.
- [Decisions](decisions): If you are interested in why Elektra uses a certain technology or strategy, then please check out the documents in this folder.
- [Markdown Link Converter](markdownlinkconverter/README.md): This tool converts links in Markdown files to make them usable in our [Doxygen documentation](https://doc.libelektra.org/api/latest/html).
- [Usecases](usecases): This folder contains use cases for our [snippet sharing service](https://www.libelektra.org/auth/login) and the upcoming web user interface for the KDB.
- [Glossary](help/elektra-glossary.md): The glossary explains common terminology used in the documentation.
