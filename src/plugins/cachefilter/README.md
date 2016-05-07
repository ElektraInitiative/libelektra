- infos = Information about the cachefilter plugin is in keys below
- infos/author = Marvin Mall <e1225943@student.tuwien.ac.at>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements =
- infos/status = experimental global
- infos/metadata =
- infos/description = A global plugin that steps in during kdbGet() process to filter the results in a way, so that no other keys than the requested one or descendants of it are returned. During kdbSet() the filtered keys are added back to the output, so that they don't get lost during the storage process. In other words, the plugin caches filtered keys so that they do not get lost by accident.

## Usage ##

There is not much to do to use the plugin. Just mount is as global plugin and you are done:
    
    kdb global-mount cachefilter
