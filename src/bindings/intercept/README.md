- infos = Information about the elektra intercept library below 
- infos/author = Thomas Waser <thomas.waser@libelektra.org> 
- infos/description =

## DESCRIPTION

When this library is loaded (either in LD_PRELOAD or ld.so) calls to `open` and `open64` will be intercepted and redirected.
Additionaly you can force files to be read-only.


## CONFIGURATION

The libraries configuration is stored under `/preload/open`.

Syntax:
`/preload/open/path\/to\/realfile = path/to/myfile`
`/preload/open/path\/to\/realfile/readonly = 1`

## INTERNALS

After the library is loaded it parses it's configuration into its internal datastructur, canonicalizes the both the real path and the new path and looks for the `open/mode` metakey.
When the an application tries to call `open` or `open64` it canonicalizes the pathname with which the function is called and looks for it in it's datastructure. If found, the pathname will be set to configured replacement path. If the read-only key is set to `1`, the WR_ONLY flag will be removed from oflags. Afterwards the real open function will be called with our values.

## EXAMPLE

```
% echo testfile > ~/testfile
% echo testreplacement > ~/testreplacement

% kdb set /preload/open
% kdb set /preload/open/\~\\/testfile "~/testreplacement"

% LD_PRELOAD=/path/to/libelektraintercept.so cat ~/testfile
testreplacement
```
