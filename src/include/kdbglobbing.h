#ifndef ELEKTRA_KDBGLOB_H
#define ELEKTRA_KDBGLOB_H

#include <kdb.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

#define ELEKTRA_GLOB_NOMATCH (-1)

int elektraKeyGlob (const ElektraKey * key, const char * pattern);
int elektraKsGlob (ElektraKeyset * result, ElektraKeyset * input, const char * pattern);

#ifdef __cplusplus
}
}
#endif

#endif // ELEKTRA_KDBGLOB_H
