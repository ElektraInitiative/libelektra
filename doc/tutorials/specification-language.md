# Specification Language

This tutorial introduces you to the concepts of the specification language which are used by Elektra. The specification language
can be used to apply a validation for the configuration that you want to ship for your application.

## How Elektra perfroms validation

One of the main concepts of Elektra are [namespaces](namespaces.md) which is also used by the specification language.
When you [mount](mount.md) a configuration such as `kdb mount test.conf /tests dump` you usually use one of the four standard namespaces 
`user`, `dir`, `proc` or `system`. 

There is a 5th namespace called the `spec` namespace and has a special behavior. Any configuration you mount under `spec` will
be used to validate configurations in other namespaces under the same path. So in the following example you can write any validation
data in the `test.spec` file and can be assured it will be validated in the other namespaces:

```shell script
# Mounts the specification 'test.spec'
# Configuration validation is now applies to all keys below the '/tests' path
kdb mount test.spec spec/tests dump

# An example configuration which will by default gets mounted to the 'user' namespace
kdb mount test.conf /tests dump type
```

Elektra takes all configuration settings which the user or admin provides and parses it into a structured internal format.
This structure is called a `KeySet` and is format agnostic (meaning that validation can be applied to any file format
such as `ini`, `yml`, etc.). After successful parsing into the KeySet, Elektra then passes this KeySet to various
validation plugins. You can get a picture of all plugins [here](/src/plugins). Every plugin performs validation 
of the KeySet depending on the metadata provided (more detail about this will come in the next section) and emits warnings
or errors. If any error happened, the changes of the configuration settings are not applied.

E.g. you can see
that in the upper example we also mounted the [type](/src/plugins/type/README.md) plugin for validation checks. This for example
can check for wrong types:

```shell script
kdb meta-set spec/tests/bool 'type' boolean
kdb set /tests/bool Batman
# Sorry, module type issued the error C03200:
# Validation Semantic: The value 'Batman' of key 'user/tests/bool' could not be converted into a boolean
```

Please note as of Elektra v0.9 you cannot add an unlimited amount of plugins. You can see if this issue
still remains in our issue tracker([#2133](https://github.com/ElektraInitiative/libelektra/issues/2133)).

## Specification writing

There are multiple ways how you can set constraints for configuration settings. All possibilities though set `metadata` to certain
configuration settings (this is also done in the spec namespace). A metadata is just additional data describing the configuration setting.
One way to set metadata is the command line tool [kdb meta-set](../help/kdb-meta-set.md) which we have used in the upper example.
For the [validation plugin](../../src/plugins/validation/README.md) which supports regular expression checks there exists even its own 
command line tool [kdb vset](../help/kdb-vset.md).

If you are writing validation for dozens of configuration settings, the command line might not be the most convenient way. You can also
write specifications in any file format of your desire. Examples can be seen [here](../../examples/spec). In Elektra, all file formats
can have metadata but some needed extra tinkering like any `yaml` parser since metadata are not natively supported. We recommend the `ini`
file format as it natively supports metadata. So the upper example
could be rewritten in an `ini` file in the following way:

```ini
[]
mountpoint = test-specification.ni
infos/plugins = type

[bool]
type = boolean
```

This could then be mounted into the spec namespace with the following command:

```shell script
kdb mount </absolute/path/to/ini/file.ini> spec/tests ni
kdb spec-mount /tests
```

The `spec-mount` command tells Elektra to load all relevant plugins. It is smart enough to automatically detect the needed plugins
by looking at all metadata. You might need to remove some metadata to not load certain plugins that cause the exceeding of the maximum
plugin number.

What remains to know are the metadata names and meanings. We have used the `type` plugin which provides the metadata `type` metadata along with
many other metadata such as `check/enum`. You can read all available metadata in the plugin READMEs which all have to have a
so called `contract` at the beginning of each file. This contract also contains the `infos/metadata` data which lists all available
metadatas. We recommend to look through those plugins READMEs to know how they are intended to use.

### Nice to know

Many plugins write metadata with arrays in them such as the `type` plugin which allows to list all enumerations for configuration settings.
We have a nice [tutorial](../tutorials/arrays.md) for you to get used to arrays.

You can also set validation for multiple configurations settings at once by using [globbing](../../src/plugins/glob/README.md).

If you decide to write metadata via a file (and especially the `ini` format) you should be aware of multiline strings that can cause
parsing issues. Descriptions of metadata often take up multiple lines and 
there are multiple `ini` format standards which tell you differently how to write them.
Elektra also has two ini plugins (`ni` and `ini`) which also handle multilines differently. We recommend the `ni` plugin which also
works correctly with our provided [examples](../../examples/spec).