- infos = Information about elektrify getenv below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/description =

elektrify-getenv(1) -- elektrify the environment of applications
================================================================

## SYNOPSIS

`elektrify-getenv` <application> <options>


## EXAMPLE

    kdb elektrify-getenv curl --elektra-version
    kdb elektrify-getenv curl http://www.libelektra.org
    kdb set system/env/override/HTTP_PROXY http://proxy.hogege.com:8000/
    kdb elektrify-getenv curl http://www.libelektra.org

By using `elektrify-getenv` the last curl invocation will use a different http proxy.


## DESCRIPTION

When an application is elektrified using libelektragetenv,
it does not only request `environ`, but also Elektra for
every getenv(3) and secure_getenv(3) library call.

Its main purpose is to:

- have a standard ways to modify the environment
- make relogin (or even restart!) of applications unnecessary
- allow a hierarchical structure for environment
- allow settings to only apply for individual applications or only in special context
- still preserve the advantages (inheriting of environment to subprocesses)
- availability of same environment in at, cron and similar scripts

It is implemented using a LD_PRELOAD technique, see [USAGE](#USAGE) below for
global activation.


## LOOKUPS

The main purpose of this approach is to finally have a well-defined
way to set and get environment variables.
Elektra's variables will be in use immediately for every newly
started application (no relogin necessary).

To do so, getenv(3) will lookup multiple sources next to searching in the environment
(environ). As running example will use `getenv("HOME") -> /path/to/home`:

1. Given commandline parameters will always be preferred (see [OPTIONS](#OPTIONS) below).

   E.g. `elektrify-getenv <app> --elektra:HOME=/path/to/home`
2. Then `/env/override/<key>` will be looked up, where <key> is the parameter to `getenv`.
   If found, the key will be returned, if it is a null keys, `getenv` will return `NULL`.

   E.g. `kdb set user/env/override/HOME /path/to/home`
3. Then environment will be requested.

   E.g. `HOME=/path/to/home elektrify-getenv <application>`
4. Then `/env/fallback/<key>` will be looked up.
   If found, the key will be returned, if it is a null keys, `getenv` will return `NULL`.

   E.g. `kdb set user/env/fallback/HOME /path/to/home`




## OPTIONS

When `elektrify-getenv` is active, every application additionally accepts
Elektra's getenv options.
Interleaving Elektra's and the application's options is allowed.
Elektra will parse its options (starting with --elektra) first and
discard them before the other application is started.
Therefore the application will not see that they even
existed, e.g.: given `elektrify-getenv <application> -V --elektra-debug -L`
the application will be called with `<application> -V -L`.

### Internal Options
 * `--elektra-help`:
   Outputs this help.
 * `--elektra-version`:
   Gives version information.
 * `--elektra-debug=file`, `ELEKTRA_DEBUG` or `/env/option/debug`:
   Trace all getenv(3) calls to a file.
   stderr if no file is given, e.g. `kdb set user/env/option/debug ""`.
   Note that null values (no forth argument), will disable debug messages.
   See examples below.
 * `--elektra-clearenv`, `ELEKTRA_CLEARENV` or `/env/option/clearenv`:
   Call clearenv(3) before entering main.
   This is a recommended security feature.
   Elektra itself, if configured that way, will still be able to use the environment.
 * `--elektra-reload-timeout=time_in_ms`, `ELEKTRA_RELOAD_TIMEOUT` or `/env/option/reload_timeout`:
   Activate a timeout based feature when a time is given in ms (and is not 0).

Internal Options are available in three different variants:

1. as commandline parameter: `--elektra-<option>`,
   which are *not* passed through exec(3) calls.
2. as environment variable: `ELEKTRA_<OPTION>`.
   which might be passed through exec(3) calls, but are removed by clearenv(3) calls.
3. as Elektra KDB entry: `/env/option/<option>`,
   which are the way to achieve an option to be enabled for every application.

   E.g. `kdb set user/env/option/clearenv ""` to clear the environment for all
   applications started by that user (note that at least `PATH` should to be set
   using `kdb set user/env/fallback/PATH "/bin:/usr/bin"` then).

   Note, that null keys are equal to non-set options.
   E.g. `kdb set system/env/option/debug "/tmp/elektra.log"` and
   `kdb set user/env/option/debug` will activate logging for the system, except
   for the current user.

### Contextual Options
 * `--elektra%<name>%=<value>` or `/env/layer/<name>`:
   Add the contextual information (=layer) `%<name>%` with its value `<value>`.
   Note that `%name%` is predefined with `argv[0]` and `%basename%` with
   `basename(argv[0])`.

Values can contain / to form hierarchies, e.g. `--elektra%name%=app/profile`

### Options for Applications
 * `--elektra:key=value`, `/env/override/<key>` or `/env/fallback/<key>`:
   set a key/value to be preferred, i.e. the first to considered as explained in [LOOKUP](#LOOKUP).

Keys can contain / to form hierarchies, e.g. `--elektra:my/HOME=/path/to/home`.



## USAGE

To always use Elektra's getenv environment, simply add the output to the file:

    elektrify-getenv | tail -1 | sudo tee -a /etc/ld.so.preload

this also can be done using Elektra:

    sudo kdb mount /etc/ld.so.preload system/ld/preload line null
    sudo kdb set "system/ld/preload/new"  `elektrify-getenv | tail -1`


## CONTEXT

The metadata `context` in the specification can be used to facilitate a context-dependent lookup.
In its metavalue all replacements of `%<name>%` will be replaced by the
given contextual options `--elektra%<name>%=<value>` and `/env/layer/<name>` keys.

E.g. to have a different home directory for any user and application:

    kdb set user/env/layer/user markus
    kdb set user/users/markus/konqueror/HOME /home/download
    kdb setmeta spec/env/override/HOME context  /users/%user%/%name%/HOME

Or to have a different lock/suspend program per computer (that all have the same config):

    kdb mount-info system/env/info            # must be below /env to be available
    kdb setmeta spec/env/layer/hostname override/#0 system/env/info/uname/nodename
    kdb setmeta spec/env/override/lock context /env/info/lock/%hostname%
    kdb set user/env/info/lock/computer1 "systemctl suspend -i
    kdb set user/env/info/lock/computer2 "xset dpms force off && xtrlock"
    `kdb getenv lock`  # call the appropriate lock method for the current computer



## BUGS

Some applications do not use `getenv(3)` or `secure_getenv(3)` for requesting the environment,
e.g. shells. This approach cannot work for them.

In the startup-phase (before main is even entered), `getenv(3)` will
not consider `/env/override/` or `/env/fallback`.

Elektra internally tries to avoid using the environment.
Some resolvers, however, use it to conform to some specifications, e.g. XDG.
Depending on the setup you use, these parameters might be used.
For more information see:

    kdb info resolver

For these parameters, `/env/override/` or `/env/fallback` will *not* be used internally, but
will be used if applications request them, too.

If you use the standard resolvers, the bug won't have any effect.

Also note that `--elektra-debug` or `ELEKTRA_DEBUG` does *not* log `getenv(3)` used by plugins
during the startup-phase.

Command line arguments apply always to the outmost command, e.g. `nice ls --elektra:COLUMNS=20`
won't have any effect because only for `nice` `COLUMNS` will be set.


## EXAMPLES

For illustration this section gives some more examples.

    elektrify-getenv man man --elektra:MANWIDTH=40

Will use MANWIDTH 40 for this invocation of man man.
This feature is handy, if an option is only available
by environment, but not by command-line arguments,
because sometimes environment variables are not trivial
to set (e.g. in Makefiles).

Debugging:

    # system wide to stderr (not recommended!):
    sudo kdb set system/env/option/debug ""
    # system wide to /var/log/elektra.log:
    sudo kdb set system/env/option/debug "/var/log/error.log"
    # but for my user to ~/.elektra.log:
    kdb set user/env/option/debug "$HOME/.elektra.log"
    # or disable it for my user:
    kdb set user/env/option/debug

Some more examples:

    kdb set user/env/override/MANOPT -- "--regex -LC"
    elektrify-getenv getenv MANOPT   # to check if it is set as expected
    kdb getenv MANOPT   # if /etc/ld.so.preload is active

Will permanently and user-wide change MANOPT to include --regex, and -LC so
that regular expressions will be used (note `man echo` will return many man
pages then) and that they will be shown in English.
This feature is handy to change the default behaviour of
applications (either system, user or directory-wide).


    kdb set system/env/override/HTTP_PROXY http://proxy.hogege.com:8000/

Will permanently and system-wide change the proxy for all applications
that honor HTTP_PROXY, e.g. w3m.
We can also link `http_proxy` to the value of `HTTP_PROXY`:

    kdb setmeta spec/env/override/http_proxy "override/#0" /env/override/HTTP_PROXY
    kdb get /env/override/http_proxy
