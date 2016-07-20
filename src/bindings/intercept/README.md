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
if the Metakey `open/mode` is set to `ro` the file will be read-only.

## INTERNALS

After the library is loaded it parses it's configuration into its internal datastructur, canonicalizes the both the real path and the new path and looks for the `open/mode` metakey.
When the an application tries to call `open` or `open64` it canonicalizes the pathname with which the function is called and looks for it in it's datastructure. If found, the pathname will be set to configured replacement path. If the read-only flag is set, the WR_ONLY flag will be removed from oflags. Afterwards the real open function will be called with our values.

## EXAMPLE

```
% echo testfile > ~/testfile
% echo testreplacement > ~/testreplacement"

% kdb set /preload/open/\~\\/testfile "~/testreplacement"

% LD_PRELOAD=/path/to/intercept.so cat ~/testfile
testreplacement
```
```
% kdb set /preload/open/\~\\/testfile system/elektra
% kdb setmeta /preload/open/\~\\/testfile "open/create" ini

% LD_PRELOAD=/path/to/intercept.so cat ~/testfile
globalplugins =
version = Below are version information of the Elektra Library you are currently using
globalplugins/postcommit = list
globalplugins/postcommit/user = list
globalplugins/postcommit/user/placements =
globalplugins/postcommit/user/placements/error = prerollback postrollback
globalplugins/postcommit/user/placements/get = pregetstorage postgetstorage postgetcleanup
globalplugins/postcommit/user/placements/set = presetstorage precommit postcommit
...
```
