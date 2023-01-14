# Regex-Example Kotlin Plugin

This plugin can be used to validate the values of keys via the `check/validation` metakey.
It uses the value of the metakey as regex expression to check if newly added values match with the stored regex.
If not, the plugin raises the `VALIDATION_SYNTACTIC` error code and prints the message from the `check/validation/message` metakey if present.

#### Mounting the plugin

To mount the plugin, use the following command:

```sh
kdb mount-java config.ni user:/test/process kdb:ni java:org.libelektra.plugin.RegexExamplePlugin
```

This will mount the file `config.ni` on mountpoint _user:/test/process_ with the KDB plugin _ni_ and the Java plugin _RegexExamplePlugin_.

#### Example usage

Ensure that newly set values for keys only contain small case letters from the English alphabet:

```sh
kdb meta-set spec:/sw/app/current/\#0/server/name check/validation "[a-z]*"
kdb meta-set spec:/sw/app/current/\#0/server/name check/validation/message "Server names must consist of lowercase letters only."
```

If now a new value is set that fulfills the regex check, no error should be raised:

```sh
kdb set -N user -- /sw/app/current/\#0/server/name aaabbbyyyzzz
```

An error should be raised if the value does not fulfil the regex:

```sh
kdb set -N user -- /sw/app/current/\#0/server/name KotlinPlugin
#> The key 'user:/sw/app/current/#0/server/name' with value 'KotlinPlugin' does not confirm to its regular expression. Reason: Server names must consist of lowercase letters only.
```

#### Current Limitations

- Only checks the correct regex during `kdbSet()` operations.
