ksNew (30,
keyNew ("system/elektra/modules/yajl",
	KEY_VALUE, "yajl plugin waits for your orders", KEY_END),
keyNew ("system/elektra/modules/yajl/exports", KEY_END),
keyNew ("system/elektra/modules/yajl/exports/get",
	KEY_FUNC, elektraYajlGet,
	KEY_END),
keyNew ("system/elektra/modules/yajl/exports/set",
	KEY_FUNC, elektraYajlSet,
	KEY_END),
keyNew ("system/elektra/modules/yajl/infos",
	KEY_VALUE, "All information you want to know", KEY_END),
keyNew ("system/elektra/modules/yajl/infos/author",
	KEY_VALUE, "Markus Raab <elektra@libelektra.org>", KEY_END),
keyNew ("system/elektra/modules/yajl/infos/licence",
	KEY_VALUE, "BSD", KEY_END),
keyNew ("system/elektra/modules/yajl/infos/description",
	KEY_VALUE,
"JSON using YAIL\n"
"\n"
"\n"
"== Introduction  ==                                          \n"
"\n"
"This is a plugin reading and writing json files              \n"
"based on the library yajl:                                   \n"
"                                                             \n"
"http://lloyd.github.com/yajl/                                \n"
"\n"
"The plugin was tested with yajl version 1.0.8-1 from Debian 6\n"
"and yajl version 2.0.4-2 from Debian 7.\n"
"\n"
"Examples of files which are used for testing can be found\n"
"below the folder in \"src/plugins/yajl/examples\" \n"
"\n"
"The json grammar can be found here:\n"
"http://www.ietf.org/rfc/rfc4627.txt\n"
"\n"
"A validator can be found here:\n"
"http://jsonlint.com/\n"
"\n"
"\n"
"\n"
"== Restrictions ==                               \n"
"                                                 \n"
"- Everything is string if not tagged by meta key \"type\"   \n"
"  Only valid json types can be used in type, otherwise there are some\n"
"  fall backs to string but warnings are produced.\n"
"- Values in non-leaves are discarded.\n"
"- Arrays will be normalized (to #0, #1, ..)\n"
"- Comments are discarded.       \n"
"\n"
"Because of these potential problems a type checker,\n"
"comments filter and directory value filter are highly recommended.\n"
"\n"
"\n"
"== TODO ==\n"
"\n"
"- empty objects are not handled\n"
"- the json file must start with map and cannot start with array\n"
"\n"
"\n"
"== Below ==\n"
"\n"
"Disclaimer: This feature is not yet implemented.\n"
"\n"
"Typically all keys are below the mountpoint. The config file itself is flat\n"
"and does not where it is mounted.\n"
"\n"
"To fake a hierarchy in a config file which actually does not exist,\n"
"the config option \"below\" can be used. With that parameter the plugin\n"
"knows to set all parameters below these parameters or to search for\n"
"any real user values below this path.\n"
"\n"
"The values in below are just searched for and discarded, they are\n"
"just needed if you do not want to shorten the names.\n"
"\n"
"  e.g. system/org/freedesktop/openicc/org/freedesktop/openicc/value\n"
"can be shortened to\n"
"  system/org/freedesktop/openicc/value\n"
"without changing the mountpoint (system/org/freedesktop/openicc)\n"
"and having the exact same config file.\n"
"\n"
"See in the example below how to use that feature in practice.\n"
"\n"
"\n"
"== OpenICC Device Config == \n"
"\n"
"\n"
"This plugin was specifically designed and tested for the\n"
"OpenICC_device_config_DB altough it is of course not limited\n"
"to it.\n"
"\n"
"Mount the plugin:                                                                                       \n"
" kdb mount OpenICC_device_config_DB.json /org/freedesktop/openicc yajl                                  \n"
"                                                                                                        \n"
"And configure so that it has the correct prefix/postfix inside the file:                                        \n"
"  kdb set system/elektra/mountpoints/_org_freedesktop_openicc/config/below org/freedesktop/openicc  \n"
"                                                                                                        \n"
"                                                                                                        \n"
"Then you can copy the OpenICC_device_config_DB.json                                                     \n"
"to systemwide or user config, e.g.                                                                      \n"
"                                                                                                        \n"
" cp src/plugins/yajl/examples/OpenICC_device_config_DB.json /etc/kdb                                    \n"
" cp src/plugins/yajl/examples/OpenICC_device_config_DB.json ~/.kdb                                      \n"
"                                                                                                        \n"
" kdb ls system/org/freedesktop/openicc                                                                  \n"
"                                                                                                        \n"
"prints out then all device entries available in the config                                              \n"
"                                                                                                        \n"
" kdb get system/org/freedesktop/openicc/device/camera/0/EXIF_manufacturer                               \n"
"                                                                                                        \n"
"prints out \"Glasshuette\" with the example config in souce                                               \n"
"                                                                                                        \n"
"You can export the whole system openicc config to ini with:                                             \n"
" kdb export system/org/freedesktop/openicc simpleini > dump.ini                                         \n"
"                                                                                                        \n"
"or import it                                                                                            \n"
" kdb import system/org/freedesktop/openicc ini < dump.ini                                               \n"
"\n"
	, KEY_END),
keyNew ("system/elektra/modules/yajl/infos/provides",
	KEY_VALUE, "storage", KEY_END),
keyNew ("system/elektra/modules/yajl/infos/placements",
	KEY_VALUE, "getstorage setstorage", KEY_END),
keyNew ("system/elektra/modules/yajl/infos/needs",
	KEY_VALUE, "", KEY_END),
keyNew ("system/elektra/modules/yajl/infos/recommends",
	KEY_VALUE, "directoryvalue comment type", KEY_END),
keyNew ("system/elektra/modules/yajl/infos/version",
	KEY_VALUE, PLUGINVERSION, KEY_END),
keyNew ("system/elektra/modules/yajl/config", KEY_END),
keyNew ("system/elektra/modules/yajl/config/",
	KEY_VALUE, "system",
	KEY_END),
keyNew ("system/elektra/modules/yajl/config/below",
	KEY_VALUE, "user",
	KEY_END),
KS_END);
