- infos = Information about base666 plugin is in keys below
- infos/author = René Schwaiger <sanssecours@me.com>
- infos/licence = BSD
- infos/provides = binary
- infos/needs =
- infos/recommends =
- infos/placements = postgetstorage presetstorage
- infos/status = discouraged experimental
- infos/metadata =
- infos/description = Base64 Encoding

# Base 666

> If You're 555 then I’m 666. What’s it like to be a heretic?

Slipknot — The Heretic Anthem

## Introduction

This plugin adds Base64 support (for the YAML CPP plugin). Most of the initial code of this plugin was copied from Peter Nirschl’s Base64 plugin. The plugin transforms keys both in the  get and set direction. For the get direction you need to add the metakey `type` with the value `binary` to the key you want to convert from a base64 encoded string to a binary value. In the set direction the plugin converts **all** binary values to base64 encoded strings. The diagram below shows how the Base64 conversion process works in conjunction with the YAML CPP plugin.

![Tree](./base666/Base64.pdf)

## Example

For a usage example, please take a look at the ReadMe of the [YAML CPP plugin](../yamlcpp).
