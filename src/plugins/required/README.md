- infos = Information about the required plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = presetstorage
- infos/status = productive maintained nodep libc final
- infos/metadata = required mandatory
- infos/description = validates configuration against set of required & accepted keys

## Introduction

Rarely you want a very strict validation where only required
keys should be accepted.

This plugin rejects every configuration where keys which are
not required are present.

In general this technique is not recommended because it
would lead to rejection of any optional parameter that is
introduced in later versions, hindering upgradability.
Nevertheless, if you need such a feature, this is the
correct plugin to do so.

## Usage

The plugin is *not* mounted by default.
So you must explicitly add it with `infos/needs`
either in specifications or contracts.

## Example

If you used [simplespeclang](/src/plugins/simplespeclang) and want to
only allow keys that are present in the specification, you can add `required`
to the `spec-mount` command:

    % kdb mount test.spec spec/test simplespeclang
    % cat << HERE > `kdb file spec/test`
    plugins required
    enum allowed = something
    HERE
    % kdb spec-mount /test
    % kdb set /test/allowed something    # is specified, works!
    % kdb set /test/rejected  something  # fails because rejected not required

