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

For information about the Web-UI, please read [here](web).

Note that the Web-UI also provides a REST service
[elektrad](web/elektrad) that allows you to get
and set configuration settings.

## Programmatic

Finally, programming languages supported via bindings can
be used to interact with Elektra.
For type safety we recommend the code generator `kdb gen`.
