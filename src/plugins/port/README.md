## Introduction

This plugin is a check plugin that checks if the port on localhost is open and a valid number.

## Purpose

One source of misconfiguration can come from invalid port numbers or from taking used ports such as 22 for ssh.
This plugin checks if the given port is open as well as the given port lies between 1-65535

## Usage

If `check/port` is specified on a given key, the plugin will validate if the port is a 
correct number between 1 and 65535.
Furthermore it will check if the port is open to use.

If `check/simpleport` is specified, only the correct port range will be checked. The port may be in use which is 
common for client applications connecting to the server.

## Alternative 1

`check/port/open true` could be used in the metadata to declare that it has to be open. A simple `check/port` will only
validate the range.

## Alternative 2

Incorporate the port plugin in the network plugin (with the same behaviour). Those plugins basically do different
things but fit both under the "network" domain.