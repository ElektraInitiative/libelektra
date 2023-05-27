# kdb-plugin-info(1) -- Print information about an Elektra plugin

## SYNOPSIS

`kdb plugin-info <plugin> [<clause name>]`<br>

Where `plugin` is the plugin in which the user would like to know information about.<br>
The optional `clause name` argument can be used to just print information from a certain clause.<br>

## DESCRIPTION

This command will print out all the information about an Elektra plugin except it's configuration.<br>
This command will also print out any functions that are exported by the plugin.<br>
Information about a plugin will be read from `system:/elektra/modules/`. If the information could not be located there, such as when a plugin is not mounted, the module will be loaded dynamically and then the information will be requested directly from the plugin.<br>
If a user wishes to load the information directly from the plugin, they can force that by using the `-l` option.<br>

## RETURN VALUES

This command returns the following exit statuses:<br>

- 0:<br>
  The command was successful.<br>

- 11:<br>
  A `clause name` was specified but not found.<br>

## OPTIONS

- `-H`, `--help`:
  Show usage of command.
- `-V`, `--version`:
  Print version info.
- `-C`, `--color <when>`:
  Print never/auto(default)/always colored output.
- `-l`, `--load`:
  Load plugin even if system:/elektra is available.
- `-c`, `--plugins-config <plugins-config>`:
  Add a plugin configuration.

## EXAMPLES

To print all the information about the `dump` plugin:<br>
`kdb plugin-info dump`<br>

To print out the license of the `resolver` plugin directly by forcing it to load:<br>
`kdb plugin-info -l resolver licence`<br>

To print out the author of the `line` plugin:<br>
`kdb plugin-info line author`<br>
