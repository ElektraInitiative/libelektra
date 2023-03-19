#ifndef ELEKTRA_PLUGIN_BASE64_FUNC_H
#define ELEKTRA_PLUGIN_BASE64_FUNC_H

#include <elektra/core.h>
#include <elektra/plugin/plugin.h>
#include <elektra/type/types.h>
#include <stdio.h>

#define ELEKTRA_PLUGIN_BASE64_PREFIX "@BASE64"
#define ELEKTRA_PLUGIN_BASE64_PREFIX_LENGTH (sizeof ("@BASE64") - 1)
#define ELEKTRA_PLUGIN_BASE64_ESCAPE "@"
#define ELEKTRA_PLUGIN_BASE64_ESCAPE_CHAR '@'

#define PLUGIN_FUNCTION_HELPER(module, function) ELEKTRA_PLUGIN_FUNCTION (function)
#define PLUGIN_FUNCTION(function) PLUGIN_FUNCTION_HELPER (ELEKTRA_PLUGIN_NAME_C, function)

// encoding functions
char * base64Encode (const kdb_octet_t * input, const size_t inputLength);
int base64Decode (const char * input, kdb_octet_t ** output, size_t * outputLength);

#endif
