- infos = Information about the python plugin is in keys below
- infos/author = Lukas Hartl <>, Leonard Guelmino <e1503940@student.tuwien.ac.at>
- infos/licence = BSD
- infos/provides = check
- infos/needs =
- infos/placements = 
- infos/status = 
- infos/description = 

## DNS Python plugin
This filter plugin checks if a Key is a valid domain name, i.e. if the name can be resolved into an IPv4 address. 
The validation takes place whenever the `kdb set` method is called.

## Usage

The python plugin requires the configuration parameter **script** holding the file path to the
python script. The mount command looks like

```sh
sudo kdb mount file.ini /python python script=/path/to/dns_plugin.py
```

For the plugin to actually check a certain key, the `check/dns` meta-key must be set.

```sh
sudo kdb meta-set user:/python/my_hostname check/dns ''
```

Now each time the key is set, the filter plugin validates the given value.

```sh
sudo kdb set user:/python/my_hostname www.libelektra.org
```
