# How-To: Logging

## Quickstart

Enable logging by compiling with the CMake option `ENABLE_LOGGER=ON` (e.g. `cmake -DENABLE_LOGGER=ON`).

By default errors and warnings are logged to stderr and syslog, while notice and info message are only logged to syslog.
Debug level message are not logged by default, but will be logged to syslog, if `ENABLE_DEBUG=ON` is set in CMake.

The file sink behaves like syslog, if it has been enabled (see below).

## Step by Step Guide

### Preparation

1. Import the logging library in your code:

   ```c
   #include <kdblogger.h>
   ```

2. Insert logging statements:

   ```c
   ELEKTRA_LOG("Hello, %s!", "🌎");
   ```

#### Log Everything

If Elektra should display all log messages, then please follow the steps below.

1. Uncomment the line:

   ```c
   // #define NO_FILTER
   ```

   in the file `src/libs/elektra/log.c`.
   (Alternatively you may define this macro via CMake.)

2. Optional: The logging levels are set in `src/include/kdblogger.h`. For example, if you want to see everything (including debug messages)
   on stderr, then change the line

   ```c
   static const int ELEKTRA_LOG_LEVEL_STDERR = ELEKTRA_LOG_LEVEL_WARNING;
   ```

   to

   ```c
   static const int ELEKTRA_LOG_LEVEL_STDERR = ELEKTRA_LOG_LEVEL_DEBUG;
   ```

   .

#### File Specific Logging

If you want to only log messages below a specific directory prefix, then please follow the steps below.

1. Search for the code:

   ```c
   #ifndef NO_FILTER
   	// XXX Filter level …
   #endif
   ```

   in the file `src/libs/elektra/log.c`.

2. Replace the code with something like:

   ```c
   #ifndef NO_FILTER
   	if (strncmp (file, "src/postfix/", sizeof ("src/postfix"))) return -1;
   #endif
   ```

   , where `src/postfix` contains all source files with logging statements that Elektra should log. For example, if you want to log everything from the `yamlcpp` plugin, then use the following code.

   ```c
   #ifndef NO_FILTER
   	if (strncmp (file, "src/plugins/yamlcpp/", sizeof ("src/plugins/yamlcpp"))) return -1;
   #endif
   ```

   . To log messages from multiple source you can use the operator `&&` to chain multiple calls to `strncmp`. For example, to log messages
   from the `directoryvalue` and `yamlcpp` plugin use the code:

   ```c
   #ifndef NO_FILTER
   	if (strncmp (file, "src/plugins/directoryvalue/", sizeof ("src/plugins/directoryvalue")) &&
   	    strncmp (file, "src/plugins/yamlcpp/", sizeof ("src/plugins/yamlcpp")))
   		return -1;
   #endif
   ```

   .

### Enabling and Disabling Sinks

The logging framework has 3 sinks: stderr, syslog and file.

The first to are enabled by default, while file is disabled. To enable it uncomment the line

```c
// #define USE_FILE_SINK
```

in `src/libs/elektra/log.c`. The file that log messages are written to is defined in this line

```c
elektraLoggerFileHandle = fopen ("elektra.log", "a");
```

.

### Compilation

1. Enable the logger: e.g. run `cmake` with the switch `-DENABLE_LOGGER=ON`

2. Build Elektra

## Log Levels

There are four log levels (ERROR is reserved for aborts within `ELEKTRA_ASSERT`):

- `ELEKTRA_LOG_WARNING`, something critical that should be shown to the user (e.g. API misuse), see #ELEKTRA_LOG_LEVEL_WARNING
- `ELEKTRA_LOG_NOTICE`, something important developers are likely interested in, see #ELEKTRA_LOG_LEVEL_NOTICE
- `ELEKTRA_LOG`, standard level gives information what the code is doing without flooding the log, see #ELEKTRA_LOG_LEVEL_INFO
- `ELEKTRA_LOG_DEBUG`, for less important logs, see #ELEKTRA_LOG_LEVEL_DEBUG
