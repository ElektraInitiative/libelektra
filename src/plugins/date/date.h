/**
 * @file
 *
 * @brief Header for date plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_DATE_H
#define ELEKTRA_PLUGIN_DATE_H

#include <kdbplugin.h>


int elektraDateGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraDateSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (date);

const char *rfc2822Strings[] = {
    "%a, %d %b %Y %T %z",
    "%d %b %Y %T %z", 
    "%a, %d %b %Y %H:%M %z", 
    "%d %b %Y %H:%M %z",
    NULL
};

const char *iso8601calendardate[] = {
    "%F", 
    "%Y%m%d", 
    "%Y-%m",
    "--%m-%d",
    "--%m%d",
    NULL
};

const char *iso8601weekdate[] = {
    "%Y-W%W",
    "%YW%w",
    "%Y-W%w-%u",
    "%YW%W%u",
    NULL
};

const char *iso8601ordinaldate[] = {
    "%Y-%j",
    "%Y%j",
    NULL
};

const char *iso8601dates[] = {
    "%F", 
    "%Y%m%d", 
    "%Y-%m",
    "--%m-%d",
    "--%m%d",
    "%Y-W%W",
    "%YW%w",
    "%Y-W%w-%u",
    "%YW%W%u",
    "%Y-%j",
    "%Y%j",
    NULL
};


const char *iso8601time[] = {
//    "%T.%3N",
//    "%T.%3N",   TODO: microseconds
    "%T",
    "%H%M%S",
    "%R",
    "%H%M",
    "%H",
    NULL
};

const char *iso8601tzdesignator[] = {
    "Z",
    "%z",
    NULL
};


#endif
