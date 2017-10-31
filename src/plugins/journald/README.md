- infos = Information about journald plugin is in keys below
- infos/author = Felix Berlakovich <elektra@berlakovich.net>
- infos/licence = BSD
- infos/provides = logging
- infos/needs =
- infos/placements = postcommit postrollback
- infos/status = maintained libc global nodoc
- infos/description = logging of committed and rolled back keys via systemd-journal

## Introduction

The plugin logs successful and failed write attempts via the systemd journal daemon (systemd-journal). 
See the [systemd-journal manpage](http://www.freedesktop.org/software/systemd/man/systemd-journald.service.html) for more information about systemd-journal.
Errors are reported with priority 3 (error priority) and use the message ID `fb3928ea453048649c61d62619847ef6`.
Successful writes are reported with priority 5 (notice priority) and use the message ID `fc65eab25c18463f97e4f9b61ea31eae`. 

## Dependencies

- `libsystemd-journal-dev`
