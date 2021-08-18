Elektra provides many ways to interact with its global key database.

## CLI

The command-line interface (CLI) is called `kdb`.
It is the commandline-tool to access and initialize the Elektra database.

It has built-in commands that can be listed via `kdb`.
Additionally, new commands can be easily extended in any language.
These external commands can be listed via `kdb list-tools`

## GUI

The graphical user interface (GUI) is called `kdb qt-gui`.

## Web-UI

For information about the Web-UI, please read [here](../../doc/tutorials/install-webui.md) and [here](./webui/README.md).

Note that the Web-UI also provides a REST service
[elektrad](elektrad/README.md) that allows you to get
and set configuration settings.

## FUSE

The Filesystem in User Space (FUSE) tool `kdb fuse` enables the inspection and modification of the KDB in the form of a classical filesystem, as it is observed by a given running process.
In the simplest case, an Elektra key appears as a file with the key name as file path and with the key value as file content.

## Programmatic

Finally, programming languages supported via bindings can
be used to interact with Elektra.
For type safety we recommend the code generator `kdb gen`.

## Creating a new tool

Tools are located in `/src/tools/` and may be created using any language.

The building and installation is performed using a standard CMake-workflow. To integrate a new tool, performs these steps:

- Register the tool in `/scripts/cmake/ElektraCache.cmake` (see `TOOLS_LIST`).
- Add `CMakeLists.txt` to `/src/tools/<newtool>`.
- Any binaries, scripts and other data needed by `<newtool>` are to be installed below `${CMAKE_INSTALL_PREFIX}`.
  (Make sure all files are installed via the install directive (excluding the CODE option) so that CPack will find all files)
  (Use a suitable value for COMPONENT; update [ElektraPackaging.cmake](/scripts/cmake/ElektraPackaging.cmake), [PackagingDebian.cmake](/scripts/cmake/Modules/PackagingDebian.cmake) and [PackagingFedora.cmake](/scripts/cmake/Modules/PackagingFedora.cmake) to configure packaging.)
- The entrypoint binary/script of the tool, i.e. an executable called `<newtool>`, needs to be installed below `${CMAKE_INSTALL_PREFIX}/${TARGET_TOOL_EXEC_FOLDER}`. Add metadata to this file as described in [kdb-find-tools(1)](/doc/help/kdb-find-tools.md). This will enable the tool to be called via `kdb <newtool>` and the proper functioning of `kdb-find-tools(1)`.
- In case dependencies for the tool are not satisfied, call `remove_tool (<newtool> "Reason")` and return.

The fuse-tool located in `/src/tools/fuse` provides a staring point for future python3-based tools.
