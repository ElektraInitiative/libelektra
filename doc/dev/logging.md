# How-To: Logging

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

2. Optional: Change the debug level in `src/include/kdblogger.h`. For example, if you want to see debug messages too, then change the line

   ```c
   ELEKTRA_LOG_LEVEL = ELEKTRA_LOG_LEVEL_INFO,
   ```

   to

   ```c
   ELEKTRA_LOG_LEVEL = ELEKTRA_LOG_LEVEL_DEBUG,
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
   	if (strncmp (file, "src/postfix/", sizeof ("src/postfix"))) goto end;
   #endif
   ```

   , where `src/postfix` contains all source files with logging statements that Elektra should log. For example, if you want to log everything from the `yamlcpp` plugin, then use the following code.

   ```c
   #ifndef NO_FILTER
   	if (strncmp (file, "src/plugins/yamlcpp/", sizeof ("src/plugins/yamlcpp"))) goto end;
   #endif
   ```

   .

### Compilation

1. Enable the logger: e.g. run `cmake` with the switch `-DENABLE_LOGGER=ON`

2. Build Elektra

## Log Levels

There are four log levels (ERROR is reserved for aborts within `ELEKTRA_ASSERT`):

- ELEKTRA_LOG_WARNING, something critical that should be shown to the user (e.g. API misuse), see #ELEKTRA_LOG_LEVEL_WARNING
- ELEKTRA_LOG_NOTICE, something important developers are likely interested in, see #ELEKTRA_LOG_LEVEL_NOTICE
- ELEKTRA_LOG, standard level gives information what the code is doing without flooding the log, see #ELEKTRA_LOG_LEVEL_INFO
- ELEKTRA_LOG_DEBUG, for less important logs, see #ELEKTRA_LOG_LEVEL_DEBUG

