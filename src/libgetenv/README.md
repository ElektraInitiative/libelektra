## Usage

To always use Elektra's environment, simply add

    /path/to/libelektragetenv.so
    /usr/lib/libelektragetenv.so  (typically)

to the file

    /etc/ld.so.preload

If you only want to use it for individual (non setuid) executions

    LD_PRELOAD=/path/to/libelektragetenv.so ls

To make sure that it works use the parameters:

    --elektra-help
    --elektra-version
