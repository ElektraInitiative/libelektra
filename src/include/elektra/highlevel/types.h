#ifndef ELEKTRA_TYPES_H
#define ELEKTRA_TYPES_H

#include <elektra/type/types.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef const char * KDBType;

extern KDBType const KDB_TYPE_STRING;
extern KDBType const KDB_TYPE_BOOLEAN;
extern KDBType const KDB_TYPE_CHAR;
extern KDBType const KDB_TYPE_OCTET;
extern KDBType const KDB_TYPE_SHORT;
extern KDBType const KDB_TYPE_UNSIGNED_SHORT;
extern KDBType const KDB_TYPE_LONG;
extern KDBType const KDB_TYPE_UNSIGNED_LONG;
extern KDBType const KDB_TYPE_LONG_LONG;
extern KDBType const KDB_TYPE_UNSIGNED_LONG_LONG;
extern KDBType const KDB_TYPE_FLOAT;
extern KDBType const KDB_TYPE_DOUBLE;
extern KDBType const KDB_TYPE_LONG_DOUBLE;
extern KDBType const KDB_TYPE_ENUM;

#ifdef __cplusplus
}
#endif

#endif // ELEKTRA_TYPES_H
