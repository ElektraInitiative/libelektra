- infos = Information about the regexdispatcher plugin is in keys below
- infos/author = e1528532 <e1528532@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides = 
- infos/recommends =
- infos/placements = presetstorage
- infos/status = maintained experimental
- infos/metadata =
- infos/description = a plugin which generates regex-representations of specification keywords

## Introduction

A plugin which generates regex-representations of specification keywords for the 
typechecker plugin before saving a configuration specification. Intended to be 
mounted before the typechecker plugin.

## Usage

In order to generate regex representations for different specification keywords, mount a configuration specification along with this plugin. 

`kdb mount <specification> spec/<path> <storage plugin to read the specification> regexdispatcher typechecker`

## Dependencies

* ghc >= 8.0.1 < 8.4
* ghc-pkg, usually bundled with ghc
* cabal, the haskell build system, usually bundled with ghc
* augeas, which provides libfa utilized by this plugin

## Limitations

The following specification keywords are supported:

* enum
* range
