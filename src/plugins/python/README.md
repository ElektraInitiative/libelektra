- infos = Information about the python plugin is in keys below
- infos/author = Manuel Mausz <manuel-elektra@mausz.at>
- infos/licence = BSD
- infos/provides =
- infos/needs =
- infos/placements = getstorage setstorage
- infos/status = maintained tested/unit configurable hook memleak
- infos/description = proxy that calls other plugins (scripts) written in python

## Introduction

The plugin uses Python to do magic things. It allows plugins to be written in Python.

What a Python script can do is not really limited by design, so any kind of plugin may be
implemented. The python plugin is especially useful to write filter and logging scripts.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-python`.

## Usage

The python plugin requires the configuration parameter **script** holding the file path to a
python script. The mount command would look like

```sh
sudo kdb mount file.ini /python python script=/path/to/filter_script.py
```

if the **ini** plugin should be used for storage and the python plugin only serves to invoke the
filter script.

For a Python script that serves as INI storage plugin itself, one uses

```sh
sudo kdb mount file.json /python python script=python_configparser.py
```

### Plugin Configuration

The python plugin supports following optional configuration values/flags:

- `script` (string): The script to be executed.
- `print` (flag): Make the plugin print engine errors, triggered by the calls of
  this plugin, to stderr. Mainly intended for diagnostic. Please note that the
  Python engine itself will print script errors to stderr regardless of this flag.
- `python/path` (string): Extends sys.path by this entry. Better then PYTHONPATH
  it is always available, regardless of the context where a binary is executed.

### Python Scripts

Python scripts must implement a class called `ElektraPlugin` with one parameter.
The class itself can implement the following functions

- `open(self, config, errorKey)`
- `get(self, returned, parentKey)`
- `set(self, returned, parentKey)`
- `error(self, returned, parentKey)`
- `close(self, errorKey)`

where _config_ & _returned_ are KeySets and _errorKey_ & _parentKey_ are Keys.
For the return codes of the functions, the same rules as for normal plugins apply.

If a function is not available, it simply is not called. A script does not have to
implement all functions therefore.

Access to **kdb** can be retrieved using the Python import

```py
import kdb
```

## Example

An example script that prints some information for each method call would be:

```py
import kdb

class ElektraPlugin(object):
    def open(self, config, errorKey):
        """
        returns
          * 0: no error
          * -1: error during initialization
        """
        print("Python script method 'open' called")
        return 0

    def get(self, returned, parentKey):
        """
        returns
          * 1: on success
          * 0: nothing was to do
          * -1: failure
        """
        print("Python script method 'get' called")
        return 1

    def set(self, returned, parentKey):
        """
        returns
          * 1: on success
          * 0: nothing was to do
          * -1: failure
        """
        print("Python script method 'set' called")
        return 1

    def error(self, returned, parentKey):
        """
        returns
          * 1: on success
          * 0: on success with no action
          * -1: failure
        """
        print("Python script method 'error' called")
        return 1

    def close(self, errorKey):
        print("Python script method 'close' called")
        return 0
```

Further examples can be found in the [python](python/) directory.

## Disclaimer

Be aware that a Python script will never be as performant as a native C/C++ plugin.
Spinning up the interpreter takes additional time and resources.
