# How-To: Logging

1. Import the logging library in your code:

   ```c
   #include <kdblogger.h>
   ```

2. Insert logging statements:

   ```c
   ELEKTRA_LOG("Hello, %s!", "🌎");
   ```

3. Uncomment the line:

   ```c
   // #define NO_FILTER
   ```

   in the file `src/libs/elektra/log.c`.

4. Enable the logger: e.g. run `cmake` with the switch `-DENABLE_LOGGER=ON`

5. Build Elektra

6. Optional: Change the debug level in `src/include/kdblogger.h`. For example, if you want to see debug messages too, then change the line

   ```c
   ELEKTRA_LOG_LEVEL = ELEKTRA_LOG_LEVEL_INFO,
   ```

   to

   ```c
   ELEKTRA_LOG_LEVEL = ELEKTRA_LOG_LEVEL_DEBUG,
   ```

   .

## Log Levels

There are four log levels (ERROR is reserved for aborts within `ELEKTRA_ASSERT`):

- ELEKTRA_LOG_WARNING, something critical that should be shown to the user (e.g. API misuse), see #ELEKTRA_LOG_LEVEL_WARNING
- ELEKTRA_LOG_NOTICE, something important developers are likely interested in, see #ELEKTRA_LOG_LEVEL_NOTICE
- ELEKTRA_LOG, standard level gives information what the code is doing without flooding the log, see #ELEKTRA_LOG_LEVEL_INFO
- ELEKTRA_LOG_DEBUG, for less important logs, see #ELEKTRA_LOG_LEVEL_DEBUG

