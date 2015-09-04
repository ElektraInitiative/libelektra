- infos = Information about dump plugin is in keys below
- infos/author = Markus Raab <elektra@libelektra.org>
- infos/description =

elektrify-getenv(1) -- elektrify the environment of applications
================================================================

## SYNOPSIS

`elektrify-getenv` <application> <options>

This application is elektrified using libelektragetenv.
This is a LD_PRELOAD technique to elektrify applications
that use getenv().

## USAGE

To always use Elektra's environment, simply add

    /path/to/libelektragetenv.so
    /usr/lib/libelektragetenv.so  (typically)

to the file

    /etc/ld.so.preload

this can be done using Elektra:

    sudo kdb mount /etc/ld.so.preload system/ld/preload line null
    sudo kdb set "system/ld/preload/new" /path/to/libelektragetenv.so

Note, that some applications do weird stuff and will not work with any LD_PRELOAD trick,
[e.g. firefox](https://bugzilla.mozilla.org/show_bug.cgi?id=435683).

If you only want to use it for individual (non setuid) executions, run

    `elektrify-getenv` <application>

## LOOKUPS

The main purpose of this approach is to finally have a well-defined
way to set and get environment variables.
Elektra's variables will be in use immediately for every newly
started application (no relogin necessary).

To do so, getenv(2) will lookup multiple sources next to searching in the environment
(environ). As running example will use `getenv("HOME")`.

1.) Given commandline parameters will always be preferred (see [OPTIONS](OPTIONS) below).
    E.g. elektrify-getenv <application> --elektra:HOME=/path/to/home
2.) Then /env/override/<key> will be looked up, where <key> is the parameter to `getenv`.
    E.g. kdb set user/env/override/HOME /path/to/home
3.) Then environment will be requested.
    E.g. HOME=/path/to/home elektrify-getenv <application>
3.) Then /env/fallback/<key> will be looked up.


## OPTIONS

You are allowed to interleave Elektra's and the application's options.
Elektra will parse its options (starting with --elektra) first and
discard them before main is started. The application will not see that they were even
there, e.g.: given <application> -V --elektra-debug -L
the application will be called with <application> -V -L

 * `--elektra-help`:
   Outputs this help.
 * `--elektra-version`:
   Gives version information
 * `--elektra-debug=file`, `ELEKTRA_DEBUG` or `env/option/debug`:
   Trace all getenv() calls file or stderr if no file is given.
 * `--elektra-clearenv`, `ELEKTRA_CLEARENV` or `env/option/clearenv`
   Call clearenv(3) before entering main.
   This is a recommended security feature.
   Elektra itself, if configured that way, will still be able to use the environment.
 * `--elektra-name=key`:
   the application name to be used instead of the executable's basename
 * `--elektra:key=value`:
   set a key/value below root to be preferred (in proc-namespace)

Note that keys can contain / to form hierarchies, e.g. `--elektra-name=app/profile`

Note that the option are available in three different variants:

1. as commandline parameter: `--elektra-<option>`,
   which are *not* passed through exec(3) calls.
1. as environment variable: `ELEKTRA_<OPTION>`.
   which might be passed through exec(3) calls, but are removed by clearenv(3) calls.
1. as Elektra KDB entry: `/env/option/<option>`,
   which are the way to achieve an option to be enabled for every application.
   e.g. `kdb set user/env/option/clearenv` to clear the environment for all
   applications started by that user (note that at least `PATH` should to be set
   using `kdb set user/env/fallback/PATH "/bin:/usr/bin"` then).



## CONTEXT

 * `/sw/env/<name>/%profile%/<key>`
 * `/env/<key>`
  will be used preferable. The spec(ification) entries are:
  - context .. do not use the key itself, but do a contextual lookup of the name, honoring layers , given in the metadata
  - no_elektra .. disable Elektra functionality for that key and only use environment
  - no_env .. disable environment fallback

 * `/env/<name>/layers`
  Specification of all layers to be activated
  Note that the profile layer??


## ENVIRONMENT

Typically Elektra tries to avoid using the environment.
Some resolvers, however, use it to be conform to some specifications, e.g. XDG.
Depending on the setup you use, these parameters might be used.
For more information see:

    kdb info resolver


Note that `--elektra-debug` does *not* log `getenv(3)` during the startup-phase.

`elektrify-getenv` provides to use environment as alternative to its options, namely:


 * `ELEKTRA_DEBUG`:
   Outputs this help.


## BUGS

Some applications do not use `getenv(3)` for requesting the environment,
e.g. most shells.
This approach does not work for them.


## EXAMPLES

For illustration this section gives some examples.

    elektrify-getenv man man --elektra:MANWIDTH=40

Will use MANWIDTH 40 for this invocation of man man.
This feature is handy, if an option is only available
by environment, but not by command-line arguments,
because sometimes environment variables are not trivial
to set (e.g. in Makefiles)-


    kdb set user/env/override/MANOPT -- "--regex -LC"

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
