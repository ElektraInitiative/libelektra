#ifndef ELEKTRA_PLUGIN_TOML_ERROR_H
#define ELEKTRA_PLUGIN_TOML_ERROR_H

#include <kdb.h>

#define ERROR_MEMORY 0xC01110
#define ERROR_INTERNAL 0xC01310
#define ERROR_SYNTACTIC 0xC03100
#define ERROR_SEMANTIC 0xC03200

void emitElektraError (Key * root, int err, const char * msg);

#endif // ELEKTRA_PLUGIN_TOML_ERROR_H
