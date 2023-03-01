- infos = Information about the python plugin is in keys below
- infos/author = Lukas Hartl <git@lukashartl.at>, Leonard Guelmino <e1503940@student.tuwien.ac.at>
- infos/licence = BSD
- infos/provides = check
- infos/status = maintained
- infos/placements = postgetstorage presetstorage
- infos/description = checks if name is resolvable

## DNS Python plugin

This filter plugin checks if a Key is a valid domain name, i.e. if the name can be resolved into an IPv4 address.
The validation takes place whenever the `kdb set` method is called.

## Usage

The python plugin requires the configuration parameter **script** holding the file path to the
python script. The mount command looks like

```sh
sudo kdb mount file.ini /python ni python script=/path/to/dns_plugin.py
```

For the plugin to actually check a certain key, the `check/dns` metakey must be set.

```sh
sudo kdb meta set spec:/python/my_hostname check/dns ''
```

Now each time the key is set, the filter plugin validates the given value.

```sh
kdb set user:/python/my_hostname www.libelektra.org
```
