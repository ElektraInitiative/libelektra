- infos = Information about the python plugin is in keys below
- infos/author = Manuel Mausz <manuel-elektra@mausz.at>
- infos/licence = BSD
- infos/provides =
- infos/needs =
- infos/placements =
- infos/status = maintained unittest configurable memleak
- infos/description = proxy that calls other plugins (scripts) written in python

## Introduction

The plugin uses Python to do magic things. It basically allows to call plugins written in Python.

What a Python script can do is not really limited by design, so any kind of plugin may be
implemented. The python plugin is especially useful to write filter and logging scripts.

## Usage

The python plugin requires the configuration parameter **script** holding the file path to a
python script. The mount command would look like

    kdb mount file.ini /python python script=/path/to/filter_script.py

if the **ini** plugin should be used for storage and the python plugin only serves to invoke the
filter script.

For a Python script that serves as (json) storage plugin itself, one could also use

    kdb mount file.json /python python script=/path/to/json_plugin.py

### Plugin Configuration

The python plugin supports following optional configuration values/flags:

- `print` (flag): Make the plugin print engine errors, triggered by the calls of
  this plugin, to stderr. Mainly intended for diagnostic. Please note that the
  Python engine itself will print script errors to stderr regardless of this flag.
- `shutdown` (value, 0 or 1): If enabled, the last call to `kdbClose()` will also
  shutdown Pythons engine. Default is 0.

### Python Scripts

Python scripts must implement a class called `ElektraPlugin` with one parameter.
The class itself can implement the following functions

- open(self, config, errorKey)
- get(self, returned, parentKey)
- set(self, returned, parentKey)
- error(self, returned, parentKey)
- close(self, errorKey)

where *config* & *returned* are KeySets and *errorKey* & *parentKey* are Keys.
For the return codes of the functions, the same rules as for normal plugins apply.

If a function is not available, it simply is not called. A script does not have to
implement all functions therefore.

Access to **kdb** can be retrieved using the Python import

    import kdb

## Example

An example script that prints some information for each method call would be:

    class ElektraPlugin(object):
        def open(self, config, errorKey):
            print("Python script method 'open' called")
            return 0

        def get(self, returned, parentKey):
            print("Python script method 'get' called")
            return 1

        def set(self, returned, parentKey):
            print("Python script method 'set' called")
            return 1

        def error(self, returned, parentKey):
            print("Python script method 'error' called")
            return 1

        def close(self, errorKey):
            print("Python script method 'close' called")
            return 0

Further examples can be found in the [python](python/) directory.

## Disclaimer

Be aware that a Python script will never be as performant as a native C/C++ plugin.
Spinning up the interpreter takes additional time and resources. The python plugin
does also not clean up resources and is not restartable.

