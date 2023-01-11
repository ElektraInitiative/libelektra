- infos = Information about RUBY plugin is in keys below
- infos/author = Bernhard Denner <bernhard.denner@gmail.com>
- infos/licence = BSD
- infos/needs =
- infos/provides = storage
- infos/placements = getstorage setstorage
- infos/recommends =
- infos/status = maintained configurable hook memleak experimental
- infos/description = Ruby plugin bridge, implement Elektra plugins with Ruby

## Introduction

This plugin is an Elektra to Ruby bridge and makes is possible to implement Elektra plugins with Ruby.
This plugin requires the `ruby` binding.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-ruby`.

## Configuration Options

The plugin has the following configuration options:

- `script`: path to the Ruby plugin to use (mandatory)

## Usage

Mount a configuration file using this plugin, which specifying the concrete Ruby Elektra plugin:

```sh
kdb mount /.ssh/authorized_keys user:/ssh/authorized_keys ruby script=<path to elektra
source>/src/plugins/ruby/ruby/ssh_authorized_keys.rb
kdb ls user:/ssh/authorized_keys
```

## Implementing Ruby Plugins

The following example illustrates how to implement a Elektra plugin with Ruby:

```ruby
require 'kdb'

# call the static function 'define' to define a new Plugin. This
# function expects one argument and a block, which implements the plugin
Kdb::Plugin.define :somename do

  # this is automatically defined
  #@plugin_name = "somename"


  # 'Open' method, called during Elektra plugin initialization
  # parameter:
  #  - errorKey: Kdb::Key, add error information if required
  # returns:
  #  - nil or 0: no error
  #  - -1      : error during initialization
  def open(errorKey)

    # perform plugin initialization
    # if an error occurs it is save to simply throw an exception. This has the same
    # semantic as returning -1
  end

  # the close method is currently not supported, since this will crash the
  # Ruby-VM if this method should be called during the VM.finalization !!!
  #def close(errorKey)
  #
  #end


  # 'check_conf' method, called before this plugin is used for mounting to check
  # if the supplied config is valid. Missing or invalid config settings can be
  # 'corrected' or added.
  # Parameter:
  #  - errorKey: Kdb::Key, to add error information
  #  - config: Kdb::KeySet, holds all plugin configuration setting
  # returns:
  #  - nil or 1 : success and the plugin configuration was NOT changed
  #  -        0 : the plugin configuration was changed and is now OK
  #  -       -1 : the configuration is NOT OK
  def check_conf(errorKey, config)

  end


  # 'get' method, is called during a get cycle, to query all keys
  #
  # Parameter:
  #  - returned : Kdb::KeySet, already holding keys to be manipulated or to be
  #               filled by a storage plugin. All added keys have to have the same
  #               key name prefix as given by parentKey.
  #  - parentKey: Kdb::Key, defining the current Elektra path. The value of this key
  #               holds the configuration file name to read from
  # Returns:
  #  - nil or 1 : on success
  #  -        0 : OK but nothing was to do
  #  -       -1 : failure
  def get(returned, parentKey)

  end


  # 'set' method, is called when writing a configuration file
  #
  # Parameter:
  #  - returned : Kdb::KeySet, holding all keys which should be written to the
  #               config file.
  #  - parentKey: Kdb::Key, defining the current Elektra path. The value of this key
  #               holds the configuration file name to write to
  # Returns:
  #  - nil or 1 : on success
  #           0 : on success with no changed keys in database
  #          -1 : failure
  def set(returned, parentKey)

  end


  # 'error' method, is called when some plugin had a problem during a write cycle
  #
  # Parameter:
  #  - returned : Kdb::KeySet, holding all keys which should be written to the
  #               config file.
  #  - parentKey: Kdb::Key, defining the current Elektra path. The value of this key
  #               holds the configuration file name to write to
  # Returns:
  #  - nil or 1 : on success
  #           0 : on success with no action
  #          -1 : failure
  def error(returned, parentKey)

  end

end
```

## Known Issues

- The plugin might cause a SIGSEGV crash on application shutdown. This is a result of missing
  RubyVM de initialization, which is silently ignored as we can't determine if the Ruby VM
  is still needed or not (libelektra might be used by an Ruby application).
- The plugin does not work correctly with Ruby version managers like `rbenv` or `rvm` and is
  intended to be used with the Ruby ecosystem installed in system context.
