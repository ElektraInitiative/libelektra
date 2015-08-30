elektrify-getenv(1) -- elektrify the environment of applications
================================================================

## SYNOPSIS

`name` <application> <options>

This application is elektrified using libelektragetenv.
This is a LD_PRELOAD technique to elektrify applications
that use getenv().

## USAGE

To always use Elektra's environment, simply add

    /path/to/libelektragetenv.so
    /usr/lib/libelektragetenv.so  (typically)

to the file

    /etc/ld.so.preload

If you only want to use it for individual (non setuid) executions

    LD_PRELOAD=/path/to/libelektragetenv.so ls


## LOOKUPS

Additional to searching in the environment (environ), getenv() will use
ksLookup() to lookup configuration.

Different lookups will be done:
1.) Commandline parameters will be preferred
2.) an application-specific lookup using /sw/env/<name>
where <name> is different for every application.
3.) a convenience fallback lookup using /env
which is the same for every application
(easies usage of already established environment-standards. In applications with a specification this should not be used. Here we make an exception, because the administrator would have to write the specification)
4.) the environment will be requested

## OPTIONS

You are allowed to interleave Elektra's and the application's options.
Elektra will parse its options (starting with --elektra) first and
discard them. The application will not see that they were even there.

 * `--elektra-help`:
   Outputs this help.
 * `--elektra-version`:
   Gives version information
 * `--elektra-name=key`:
   the application name to be used instead of the executable's basename
 * `--elektra:key=value`:
   set a key/value below root to be preferred (in proc-namespace)

Note that keys can contain / to form hierarchies.

Every option starting with --elektra will be discarded from argv
before the application's main function is started.

## KDB

 * `/sw/env/<name>/%profile%/<key>`
 * `/env/<key>`
  will be used preferable. The spec(ification) entries are:
  - context .. do not use the key itself, but do a contextual lookup of the name, honoring layers , given in the metadata
  - no_elektra .. disable Elektra functionality for that key and only use environment
  - no_env .. disable environment fallback

 * `/sw/env/<name>/layers`
  Specification of all layers to be activated
  Note that the profile layer??


## ENVIRONMENT

 * ELEKTRA_DEBUG:
   debug output


## EXAMPLES

    elektrify-getenv man man --elektra:MANWIDTH=40

Will use MANWIDTH 40 for this invocation of man man.
This feature is handy, if an option is only available
by environment, but not by command-line arguments,
because sometimes environment variables are not trivial
to set (e.g. in Makefiles)-


    kdb set user/sw/env/man/MANOPT -- --regex

Will permanently and user-wide change MANOPT to include --regex, so that -K
and similar options automatically prefer regular expressions.
This feature is handy to change the default behaviour of
applications (either system, user or directory-wide).


    kdb set system/env/HTTP_PROXY http://proxy.hogege.com:8000/

Will permanently and system-wide change the proxy for all applications
that honor HTTP_PROXY, e.g. w3m.
