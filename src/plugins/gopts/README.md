- infos = Information about the gopts plugin is in keys below
- infos/author = Klemens Böswirth <k.boeswirth+git@gmail.com>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = procgetstorage
- infos/status = recommended productive maintained nodep libc experimental
- infos/metadata =
- infos/description = Parses command-line options using elektra-opts

## Introduction

This plugin is very simple in what it does. It uses system specific methods of accessing `argc` and `argv` outside of `main` and then calls
`elektraGetOpts` to do options processing.

## Usage

TODO

## Examples

TODO

## Limitations

- `infos/placements` should actually be between getstorage and postgetstorage. There should be a position procgetstorage that should be used
  by all plugins that only write to the `proc` namespace.
