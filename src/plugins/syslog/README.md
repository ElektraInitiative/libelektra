- infos = Information about the syslog plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = logging
- infos/needs =
- infos/placements = pregetstorage postcommit postrollback
- infos/status = maintained nodep hook
- infos/description = Logs set and error calls to syslog.

## Introduction

This plugin is a logging plugin which adds a log entry to syslog on
commit and rollback of the configuration.

Configure the plugin with `log/get=1` to enable logging when configuration is
loaded. For example, `kdb gmount syslog log/get=1`.

## Installation

See [installation](/doc/INSTALL.md).
The package is called `libelektra5-extra`.
