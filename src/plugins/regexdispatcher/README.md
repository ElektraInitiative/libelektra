- infos = Information about the regexdispatcher plugin is in keys below
- infos/author = Armin Wurzinger <e1528532@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = presetstorage
- infos/ordering = typechecker
- infos/status = maintained experimental
- infos/metadata = check/enum/# check/enum/multi check/range check/validation
- infos/description = a plugin which generates regex-representations of specification keywords

## Introduction

A plugin which generates regex representations of specification keywords for the
typechecker plugin before saving a configuration specification. For instance the
regex for the metakey `check/enum/#` depends on the metakey's values, as the two
definitions `check/enum/#1=A` and `check/enum/#2=B` would result in the regex
`A|B` stored in the `elektra/spec/regex/check/enum` metakey that can be treated
by the typechecker as a regex directly.

## Usage

In order to generate regex representations for different specification keywords, mount a configuration specification along with this plugin.

`kdb mount <specification> spec/<path> <storage plugin to read the specification> regexdispatcher typechecker`

## Dependencies

- ghc >= 8.0.1 < 8.4
- ghc-pkg, usually bundled with ghc
- cabal, the haskell build system, usually bundled with ghc
- augeas, which provides libfa utilized by this plugin

## Supported Metakeys

The following specification keywords are supported:

- check/enum/# and check/enum/multi
- check/range
