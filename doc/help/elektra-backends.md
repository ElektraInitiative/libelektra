# elektra-backends(7) -- the backend concept

Elektra has introduced **backends** to support the storage of
key databases in different formats.
Elektra abstracts configuration so that applications can receive
and store settings without carrying information
about how and where these are actually stored.
It is the purpose of the backends to implement these details.

Since Elektra 0.8 a backend is composed of many plugins.

## MULTIPLE PLUGINS

It's clear that too many features in one backend are problematic. It introduces unwanted external dependencies and leads to
less portable backends. Many different aspects clutter the code making
the backends unmaintainable. Features of other backends cannot be taken
with ease because they are interwoven with other code.

To support the reuse of functionality, they must be useful in different
situations. Separation of concerns is required for that. Each backend
needs to have a specific purpose rather than implementing the full
semantics of Elektra.

It was impossible to implement powerful and feature-rich backends so far
because of the lack of modularity. Desirable features like notification
and type checking have always been in the developers' heads, but there was
no place where it would fit in without making the system unmaintainable,
complex and full of unwanted external dependencies.

To solve this dilemma, Elektra uses **multiple plugins** together to
build up a backend. The key set processed by one plugin will be passed
to the next. This approach allows us to implement separate features
in separate plugins. Plugins concerned with reading from and writing to
permanent storage are called **storage plugins**.
We cleaned existing backends from other tasks so that their only job
now is related to the storage. Using this approach, the plugins provide
the desired separation of concerns inside a backend.

Each plugin implements a single concrete requirement and it does that
well. This architecture allows plugins to have external
dependencies. Not every plugin has the burden to be portable anymore.
That is no problem because the plugins are separate subprojects and
maintainers can decide if they should be built for a specific platform
or not. Afterward, users can choose which plugins they want to install
and use. And finally, the administrator can choose which of the plugins
should be loaded for each backend. If a specific feature is not needed,
it is not included and does not cause additional overhead. Given the
chosen approach, the core of Elektra can stay minimal.
Most functionality can be moved to plugins.

Now let us look at the **development time** with multiple plugins. The
programmer will find many reusable plugins. Some of them already fulfil
given requirements. While checking the code quality of the plugins,
the programmer actually learns how the plugin works. Given that point
of view, the programmer will decide to use Elektra because he or she
can choose from a pool of existing plugins.

## SEE ALSO

- The tool for mounting a backend is [kdb-mount(1)](kdb-mount.md)
