- infos = Information about the gopts plugin is in keys below
- infos/author = Klemens Böswirth <k.boeswirth+git@gmail.com>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = procgetstorage
- infos/status = recommended productive maintained nodep libc
- infos/metadata = args args/index command opt opt/long opt/arg opt/flagvalue opt/help opt/hidden opt/# opt/#/long opt/#/arg opt/#/flagvalue opt/#/hidden env env/#
- infos/description = Parses command-line options using elektra-opts

## Introduction

This plugin is very simple in what it does. It uses system specific methods of accessing `argc` and `argv` outside of `main` and then calls
`elektraGetOpts` to do options processing.

The config key `/offset` may be given to ignore a number of arguments at the start of `argv`. This is useful, if `gopts` was called from a
script via an interpreter (e.g. in python scripts).

## Usage

The preferred way of using this plugin is via `kdbEnsure`:

```c
KDB * kdb = kdbOpen (parentKey);

KeySet * contract = ksNew (1, keyNew ("system:/elektra/ensure/plugins/global/gopts", KEY_VALUE, "mounted", KEY_END), KS_END);
int rc = kdbEnsure (kdb, contract, parentKey);
if (rc != 0)
{
	// error handling
}

// gopts now mounted
KeySet * ks = ksNew (0, KS_END);
kdbGet (kdb, ks, parentKey);
```

## Help message

If `elektraGetOpts` determined that the application is in help mode (i.e. `--help` was used), `gopts` will set the key
`proc:/elektra/gopts/help` to `1`. It will also invoke `elektraGetOptsHelpMessage` and write the result into the key
`proc:/elektra/gopts/help/message`. The `usage` and `prefix` values used for this call can be configured, via the
`/help/usage` and `/help/prefix` config keys.
