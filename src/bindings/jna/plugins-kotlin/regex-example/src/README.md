# Regex-Example Kotlin Plugin

This plugin can be used to validate the values of keys via the `check/ktex-regex` metakey.

It uses the value of the meta key as regex expression to check if newly added values match
with the stored regex. If not, the plugin raises the `VALIDATION_SYNTACTIC` error code.

#### Example usage

Ensure that newly set values for keys only contain small case letters from the english alphabet:

```sh
kdb meta-set spec:/sw/app/current/\#0/server/name check/ktex-regex "[a-z]*"
```

If now a new value is set that fulfills the regex check, no error should be raised:

```sh
kdb set -N user -- /sw/app/current/\#0/server/name aaabbbyyyzzz
```

An error should be raised if the value does not fulfil the regex:

```sh
kdb set -N user -- /sw/app/current/\#0/server/name KotlinPlugin
#> Found key with regex set which does not match user:/sw/app/current/#0/server/name: KotlinPlugin
```

#### Current Limitations

- Only checks the correct regex during `kdbSet()` operations.
