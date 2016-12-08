- infos = Information about the curlget plugin is in keys below
- infos/author = Thomas Waser <thomas.waser@libelektra.org>
- infos/licence = BSD
- infos/needs =
- infos/provides =
- infos/recommends =
- infos/placements = getresolver setresolver
- infos/status = unittest configurable readonly preview unfinished nodoc
- infos/metadata =
- infos/description = mount remote config files via curl

## Example ##

    cat /tmp/curltest.ini
    cat: /tmp/curltest.ini: No such file or directory

    kdb mount -R noresolver /tmp/curltest.ini system/curltest ini curlget url="https://raw.githubusercontent.com/ElektraInitiative/libelektra/master/src/plugins/ini/ini/plainini"

    kdb ls system/curltest
    system/curltest/nosectionkey
    system/curltest/section1
    system/curltest/section1/key1
    system/curltest/section1/key2
    system/curltest/section2
    system/curltest/section2/emptykey
    system/curltest/section2/key3

    cat /tmp/curltest.ini
    nosectionkey = nosectionvalue
    [section1]
    key1 = value1
    key2 = value2
    [section2]
    emptykey =
    key3 = value3

