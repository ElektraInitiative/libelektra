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

This plugin validates email addresses using regular expressions.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-experimental`.

## Usage

```sh
# Mount `email` plugin to cascading namespace `/tests/email`
kdb mount config.dump /tests/email dump


# Check the validity of the email stored in `/tests/email/adr`
kdb meta-set /tests/email/adr check/email

# Set a correct email address
kdb set /tests/email/adr test+email@dev.libelektra.com
kdb get /tests/email/adr
#> test+email@dev.libelektra.com

# Try to set incorrect addresses
kdb set /tests/email/adr invalid.address@com
# RET: 0
kdb set /tests/email/adr bad@-email
# RET: 0
kdb set /tests/email/adr .bad@email.com
# RET: 0

# Undo modifications to the database
kdb rm -r /tests/email
kdb umount /tests/email
```

## Limitations

The plugin only checks email addresses for validity. It is not able to resolve if the host and or check if the address can receive emails.
The validation does not completely support the RFC5321. The following valid email addresses are not supported:
- “(ele)ktra@elektra.io”
- “elektra@elektra.io(io)”
- “"hi@you"@elektra.io”
- “"hi you"@elektra.io”
- “" "@elektra.io”
- “"<\"@\\".!.#%\$@@elektra.io”
- “cow@[dead::beef]”
- “是是@是是.是是”
- “1@[23456789]”

There is no validation of top level domains. The following invalid email addresses will be allowed:
“valid+part@nonexistenttopleveldomain”
