# Why should I use Elektra? #

The three main points relevant for most people are:

1. Even though Elektra provides a global key database,
   that allows *read- and write access* of every single
   parameter *for any application* in an integrated fashion,
   configuration files stay human read- and writable.
2. Flexible adoption on how the configuration is accessed
   via plugins: you can run arbitrary code in multiple
   languages or notify others when configuration files
   are changed. [Plugins](/src/plugins) allow us to
   support hundreds of different configuration file
   formats.
3. Elektra allows us to specify configuration values:
   - use the value of other configuration values (symbolic links)
   - calculate the values based on other configuration values
   - [validate configuration files](/doc/tutorials/validation.md)
   - [generate code based on specifications](/src/tools/gen)
   - [and much more](/src/plugins/README.md)


## Why not other solutions? ##

Some might ask: isn't this solution overkill?
Why not tackle these three issues separately?
The answer is:

1. If we would only implement a configuration file library for
   applications, we would hinder the work of administrators and
   would not provide **external access** to configuration.
2. If we only implement an administrator tool that can parse
   and generate configuration files, but is not used by the
   applications themselves, we create a gap that leads to
   inconsistent understanding of the configuration **syntax**.
3. If we only specify the meaning of configuration within
   applications but not in a form accessible for administrators,
   we would create a gap that leads to inconsistent understanding
   of the configuration **semantics**.

For common understanding of syntax and semantics of configuration files
a full-stack solution like Elektra is required. We acknowledge, however,
that such a change cannot be done overnight, thus we integrate as many
configuration file formats as possible. This way, people can continue
using files in `/etc`, regardless of whether or not Elektra is used.


To give one example, in OpenLDAP 2.4.39 the value of `listener-threads`
will be reduced to the next number that is a power of 2. To correctly
manipulate the setting we need not only know the *syntax* of how to write
listener-threads correctly in the configuration file, but also the
knowledge how the value is transformed internally. Elektra solves all
three issues, and then users can easily **externally** manipulate
`listener-threads`, without caring about the concrete **syntax** of the
file and getting feedback of the **semantics** (you might get validation
errors and you can receive the value exactly as the application will get it).


## Who should use Elektra? ##

Elektra targets different kinds of users:

1. Developers to develop a better integrated free software ecosystem and
   plugins for better validation/notification.
   If you have an application that reads or writes configuration files,
   you are in the target group.
2. System administrators to have a higher-level view of their configuration
   and enable them to better consider context.
   If you want to specify or automate configuration tasks, you are in the
   target group.

## Unique Features ##

Features that rarely can be found elsewhere (at least in this combination):

- Bootstrap code and proper abstraction is included:
  - You do not need to worry about the file names of configuration files
    in the application.
  - Cascading between `/etc`, `$HOME`, `cwd` and so on.
  - You can change which Elektra path is connected to which configuration
    file with [mounting](/doc/help/elektra-mounting.md).
  - Portable across OS (Linux, BSD, w64, mac os x,.. ) and desktop systems (GNOME, KDE,...).
- No daemon, so no single point of failure but still having guarantees of consistent,
  validated files with good performance.
- Provides 3-way merging for configuration upgrades.

## Further Reasons ##

- Links and automatic calculation of values:
   unlike with other solutions you do not need to duplicate
   configuration values for different applications but
   you can comfortably link between them which makes
   many inconsistencies impossible.
- Allows us to easily create GUIs and web-UIs for the whole configuration
  on the system.
- Allows you to import/export all parts of the configuration.
- Syntax independence: you can consistently use your favourite syntax.
- Configuration Management (such as Puppet) can be used on top of it
  without having to fiddle with specifics of every configuration file.
- CLI-tool available
- `kdb editor` allows you to edit any path of Elektra with your favourite
  syntax (independent of the actual syntax of the configuration files
  that store values of this path).
- Allows us to also (integrate commandline arguments and environment)[/src/libs/getenv]
  into a consistent place for configuration.
- Reduces huge amount of code: Nearly every application has very similar code:
  - finding the correct configuration file (for different OS)
  - parsing configuration files
  - validating configuration files
  - replace configuration files on changes
- All advantages libraries have:
  - Performance: Improvements in the library benefits all applications.
  - The library only needs to be loaded once in the memory.
  - On fixes not all binaries of all applications need to be replaced.
- All advantages maintained code with a community has:
  - If something does not work, open an issue.
  - If you have a question, open an issue.
  - Regular releases.


## Further Readings ##


- Get a [big picture](BIGPICTURE.md)
- Look into [the glossary](/doc/help/elektra-glossary.md).
- For more about tools go on reading [here](/doc/help/kdb.md).
- Another viewpoint [why to use Elektra is described here](/doc/help/elektra-introduction.md)

