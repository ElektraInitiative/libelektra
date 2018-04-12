- infos = Information about the template plugin is in keys below
- infos/author = Klemens Böswirth <k.boeswirth+git@gmail.com>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = nodep experimental unfinished
- infos/metadata =
- infos/description = interprets strings with prefix 0x as hexadecimal numbers

## Introduction

This plugin can be used to read configuration files that use hexadecimal values. All hexadecimal values (strings starting with 0x) will be 
converted into decimal when the Elektra reads values from the mounted file. When Elektra writes back to the file all values originally 
stored as hexadecimal will be converted back and stored as before.

## Usage

- To mount a simple backend that uses hexadecimal numbers, you can use:

    ```sh
    sudo kdb mount test.ecf /examples/hexnumber/test hexnumber
    ```

- To unmount the plugin use the following command:

    ```sh
    sudo kdb umount /examples/hexnumber/test
    ```

