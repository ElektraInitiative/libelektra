#ifndef ELEKTRA_PLUGIN_BASE64_FUNC_H
#define ELEKTRA_PLUGIN_BASE64_FUNC_H

#include <kdbplugin.h>
#include <kdbtypes.h>
#include <stdio.h>

#define ELEKTRA_PLUGIN_BASE64_PREFIX "@BASE64"
#define ELEKTRA_PLUGIN_BASE64_ESCAPE "@"
#define ELEKTRA_PLUGIN_BASE64_ESCAPE_CHAR '@'

// encoding functions
char * ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, base64Encode) (const kdb_octet_t * input, const size_t inputLength);
int ELEKTRA_PLUGIN_FUNCTION (ELEKTRA_PLUGIN_NAME_C, base64Decode) (const char * input, kdb_octet_t ** output, size_t * outputLength);

#endif
