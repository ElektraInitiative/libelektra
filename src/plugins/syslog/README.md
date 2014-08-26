- infos = Information about the syslog plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = logging
- infos/placements = postcommit postrollback
- infos/description = Logs set and error calls to syslog.

## Introduction ##

This plugin is a logging plugin which adds a log entry to syslog on certain methods. Whenever a set or an error call occurs within Elektra it adds a new log entry to syslog about the call.
