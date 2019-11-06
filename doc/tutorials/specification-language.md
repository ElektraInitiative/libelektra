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
or errors. 

E.g. you can see
that in the upper example we also mounted the [type](/src/plugins/type/README.md) plugin for validation checks. This for example
can check for wrong types:

```shell script
kdb meta-set spec/tests/bool 'type' boolean
kdb set /tests/bool Batman
# Sorry, module type issued the error C03200:
# Validation Semantic: The value 'Batman' of key 'user/tests/bool' could not be converted into a boolean
```

Please note as of Elektra v0.9 you cannot add an unlimited amount of validation plugins. You can see if this issue
still remains in our issue tracker. See [#2133](https://github.com/ElektraInitiative/libelektra/issues/2133).