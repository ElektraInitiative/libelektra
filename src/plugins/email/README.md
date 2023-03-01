- infos = Information about the email plugin is in keys below
- infos/author = Andreas Kraschitzer <e01226853@student.tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides = check
- infos/recommends =
- infos/placements = presetstorage
- infos/status = maintained unittest nodep experimental
- infos/metadata = check/email
- infos/description = Validation for email addresses

# Email Address Validation

## Introduction

This plugin validates email addresses using regular expressions. The plugin does not support the full RFC5321 spec. For more information see [Limitations](#Limitations) below.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-experimental`.

## Usage

```sh
# Mount `email` plugin to cascading namespace `/tests/email`
kdb mount config.dump /tests/email dump email

# Incorrect address is valid with incomplete configuration
# kdb meta set spec:/tests/email/noaddr check/email
kdb set user:/tests/email/noaddr invalid..address@com
# RET: 0

# Check the validity of the email stored in `/tests/email/adr`
kdb meta-set spec:/tests/email/adr check/email ""

# Set a correct email address
kdb set user:/tests/email/adr test+email@dev.libelektra.com
kdb get user:/tests/email/adr
#> test+email@dev.libelektra.com

# Try to set incorrect addresses
kdb set user:/tests/email/adr invalid..address@com
# STDERR: .*Validation Semantic.*
# ERROR:  C03200
# RET: 5

kdb set user:/tests/email/adr not.@email.com
# STDERR: .*Validation Semantic.*
# ERROR:  C03200
# RET: 5

kdb set user:/tests/email/adr @
# STDERR: .*Validation Semantic.*
# ERROR:  C03200
# RET: 5

# Undo modifications to the database
kdb rm -rf user:/tests/email
kdb rm -rf spec:/tests/email
kdb umount /tests/email
```

## Limitations

The plugin only checks email addresses for validity. It is not able to resolve if the host and or check if the address can receive emails.
The validation does not completely support the RFC5321. The following valid email addresses are not supported:

- `(ele)ktra@elektra.io`
- `elektra@elektra.io(io)`
- `"hi@you"@elektra.io`
- `"hi you"@elektra.io`
- `" "@elektra.io`
- `"<\"@\\".!.#%\$@@elektra.io`
- `cow@[dead::beef]`
- `1@[23456789]`

There is no validation of top level domains and no length check. The following invalid email addresses will be allowed:

- `valid+part@nonexistenttopleveldomain`
- `1234567890123456789012345678901234567890123456789012345678901234+x@example.com`
