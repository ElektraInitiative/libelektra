- infos = Information about the recorder plugin is in keys below
- infos/author = Maximilian Irlinger <max@maxirlinger.at>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = hook
- infos/status = recommended productive maintained conformant
- infos/metadata =
- infos/description = provides session recording functionality

## Introduction

This plugin's sole responsibility is to act as a hook plugin for the `libelektra-record` library.
For internal use by `libelektra-kdb` only.
Intentionally not documented here.

## Plugin Configuration

You can specify the path and name of the lockfile with the `lockfile` parameter.
Default is `/tmp/elektra_record.lock`.

## Dependencies

This plugin depends on the `libelektra-record` library.
