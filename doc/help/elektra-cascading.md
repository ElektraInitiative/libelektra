elektra-cascading(7) -- of key names
====================================

**Cascading** is the triggering of secondary actions.
For configuration it means that first the user configuration is read
and if this attempt fails, the system configuration is used as fallback.

The idea is that the application installs a configuration storage
with default settings that can only be changed by the administrator.
But every user has the possibility to override parts of this *system
configuration* regarding the user's needs in the *user configuration.
To sum up, besides system configuration, users have their own key
databases that can override the settings according to their preferences.

Thus when a key starts with `/` such cascading will automatically
performed.

See [application integration](/doc/tutorials/application-integration.md)
for how to use cascading names in the context of applications.

[Read more about namespaces.](/doc/help/elektra-namespaces.md)
