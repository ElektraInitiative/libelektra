- infos = All information you want to know
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/provides = storage/info
- infos/needs =
- infos/recommends =
- infos/placements = setstorage getstorage
- infos/status = maintained nodep libc readonly limited nodoc concept
- infos/description = includes constants information into kdb

## Introduction

Includes constants information into the key database. The constants are defined during CMake build.

The plugin is readonly.

## Usage

To mount it, use

    kdb mount -R noresolver none system/constants constants

To list all constants, use:

    kdb ls system/constants

