elektrify-getenv(1) -- elektrify the environment of applications
================================================================

## SYNOPSIS

`name` <application> <options>

## Usage

To always use Elektra's environment, simply add

    /path/to/libelektragetenv.so
    /usr/lib/libelektragetenv.so  (typically)

to the file

    /etc/ld.so.preload

If you only want to use it for individual (non setuid) executions

    LD_PRELOAD=/path/to/libelektragetenv.so ls

## OPTIONS

You are allowed to interleave Elektra's and the application's options.
Elektra will parse its options (starting with --elektra) first and
discard them. The application will not see that they were even there.

 * `--elektra-help`:
   Outputs this help.
 * `--elektra-version`:
   Gives version information

## ENVIRONMENT

 * ELEKTRA_DEBUG:
   debug output
