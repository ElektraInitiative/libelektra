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
