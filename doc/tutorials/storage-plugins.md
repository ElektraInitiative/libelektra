# How-To: Write a (Well Behaved) Storage Plugin

The [plugin tutorial](plugins.md) already covers some of the most interesting parts on how to write a (storage) plugin. This text will tell you a little bit more about how a storage plugin should act. While it is usually relatively easy to create a plugin that stores basic key-value pairs, adding advanced features such as support for

- [arrays](arrays.md), and
- [metadata](../dev/metadata.md),

takes more work. Before you continue with this text, please make sure that you read all of the linked documents.

## Don’t Add Additional Keys

One common problem of storage plugins is, that they store too many keys. For example, if the user adds the keys

- `user/tests/storage/root` and
- `user/tests/storage/root/level1/level2/level3`,

then your plugin should only store those two keys. **Do not** add the keys

- `user/tests/storage/root/level1`, or
- `user/tests/storage/root/level1/level2`

to the key set. One plugin that handles this situation properly is [YAML CPP](/src/plugins/yamlcpp/), as the following [Markdown Shell Recorder test](https://master.libelektra.org/tests/shell/shell_recorder/tutorial_wrapper) shows:

```sh
# Mount plugin
sudo kdb mount config.yaml user/tests/storage yamlcpp

# Add key-value pairs
kdb set user/tests/storage/root 🐓
kdb set user/tests/storage/root/level1/level2/level3 🐣

# Make sure that YAML CPP did not store any additional keys
kdb ls user/tests/storage/root
#> user/tests/storage/root
#> user/tests/storage/root/level1/level2/level3

# Undo modifications to the key database
sudo kdb umount user/tests/storage
```

.

<!--
TODO: Describe difference between keys that store null values (binary data) and empty values (string data)
TODO: Describe how to store arrays properly (`array` metadata): Just add a link to `arrays.md`
TODO: Add information on how plugins should store comment (meta)data
TODO: Document that a plugin should keep the ordering of key-value pairs of a document intact, when writing data back to the configuration file
-->
