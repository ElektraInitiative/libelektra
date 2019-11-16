- infos = Information about the Elektra intercept library below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/status = experimental maintained
- infos/provides = intercept
- infos/description = File system interception

## DESCRIPTION

When this library is loaded (either in LD_PRELOAD or ld.so) calls to `open` and `open64` will be intercepted and redirected.
Additionally, you can force files to be read-only, or even generated from Elektra backends.

## CONFIGURATION

The libraries configuration is stored under `/elektra/intercept/open`.

Syntax:

```
/elektra/intercept/open/path\/to\/realfile = path/to/myfile
/elektra/intercept/open/path\/to\/realfile/readonly = 1
/elektra/intercept/open/path\/to\/realfile/generate = system:/info
/preload/open/path/\to\/realfile/generate/plugin = ini
```

## INTERNALS

After the library is loaded it parses its configuration into its internal data structure, canonicalizes both the real path and the new path and looks for the `open/mode` metakey.
When an application tries to call `open` or `open64` it canonicalizes the pathname with which the function is called and looks for it in it's data structure. If found, the pathname will be set to configured replacement path. If the read-only key is set to `1`, the WR_ONLY flag will be removed from oflags. Afterwards the real open function will be called with our values.
If the `/generate` and `/generate/plugin` keys are set, the library will generate a configuration from the backend pointed to by `/generate` using the storage plugin specified in `/generate/plugin`

## EXAMPLE

```
% echo testfile > ~/testfile
% echo testreplacement > ~/testreplacement

% kdb set /elektra/intercept/open
% kdb set /elektra/intercept/open/\~\\/testfile "~/testreplacement"

% kdb elektrify-open cat ~/testfile
testreplacement

% kdb mount-info
% kdb set /elektra/intercept/open/\~\\/testfile/generate "system:/info/uname"
% kdb set /elektra/intercept/open/\~\\/testfile/generate/plugin ini

% kdb elektrify-open cat ~/testfile
= none
machine = x86_64
nodename = Skipper
release = 4.6.0-1-amd64
sysname = Linux
version = "#1 SMP Debian 4.6.2-2 (2016-06-25)"

```

## Limitations

This binding is experimental, please be careful.

No write support of configuration files (back to Elektra).
