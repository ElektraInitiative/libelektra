## Why should I use Elektra? ##

The three main points relevant for most people are:

1. Even though Elektra provides a global keydatabase
   configuration files stay human read- and writable
   which allows us to integrate unmodified software.
2. Flexible adoption on how the configuration is accessed
   via plugins: you can run arbitrary code, e.g. do a
   `git commit` or log/notify when configuration files
   are changed.
3. Elektra allows you to specify configuration values:
   - use the value of other configuration values (symbolic links)
   - calculate the values based on other configuration values
   - validation configuration files
   - [generate code based on it](/src/tools/gen)
   - [and much more](/src/plugins/README.md)


### Further reasons ###

- Bootstrap code and proper abstraction is included:
  - you do not need to worry about the file names of configuration files
    in the application
  - cascading between `/etc`, `$HOME`, `cwd` and so on
  - you can change which Elektra path is connected to which configuration
    file with [mounting](/doc/help/elektra-mounting.md)
- Links and automatic calculation of values:
   unlike with other solutions you do not need to duplicate
   configuration values for different applications but
   you can comfortably link between them which makes
   many inconsistencies impossible.
- Allows us to easily create GUIs and web-UIs for the whole configuration
  on the system.
- Allows you to import/export all parts of the configuration.
- Provides 3-way merging for configuration upgrades.
- Syntax independence: you can consistently use your favourite syntax.
- Performance: Improvements in the library benefits all applications.
  The library only needs to be loaded once in the memory.
- Configuration Management (such as Puppet) can be used on top of it
  without having to fiddle with specifics of every configuration file.
- CLI-tool available
- `kdb editor` allows you to edit any path of Elektra with your favourite
  syntax (independent of the actual syntax of the configuration files
  that store values of this path).
- Allows us to also (integrate commandline arguments and environment)[/src/libs/getenv]
  into a consistent place for configuration.
