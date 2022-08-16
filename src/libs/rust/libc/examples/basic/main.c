#include <stdio.h>

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>


typedef enum elektraNamespace {
  KEY_NS_NONE = 0,
  KEY_NS_CASCADING = 1,
  KEY_NS_META = 2,
  KEY_NS_SPEC = 3,
  KEY_NS_PROC = 4,
  KEY_NS_DIR = 5,
  KEY_NS_USER = 6,
  KEY_NS_SYSTEM = 7,
  KEY_NS_DEFAULT = 8,
} elektraNamespace;

typedef int elektraKeyFlags;

typedef int elektraKeySetFlags;

typedef struct CKeySet {
  const struct CKey **array;
  size_t size;
  size_t alloc;
  struct CKey *cursor;
  size_t current;
  elektraKeySetFlags flags;
  uint16_t refs;
  uint16_t reserved;
} CKeySet;

typedef struct CKey {
  void *data;
  size_t dataSize;
  char *key;
  size_t keySize;
  char *ukey;
  size_t keyUSize;
  size_t ksReference;
  elektraKeyFlags flags;
  struct CKeySet *meta;
} CKey;

typedef unsigned int elektraCopyFlags;

typedef int elektraLockFlags;

ssize_t elektraKeyAddBaseName(struct CKey *key, const char *baseName);

ssize_t elektraKeyAddName(struct CKey *key, const char *addName);

/**
 * You have to free the returned string manually, otherwise there will be memory leaks
 */
char *elektraKeyBaseName(const struct CKey *key);

ssize_t elektraKeyBaseNameSize(const struct CKey *key);

int elektraKeyClear(struct CKey *key);

int elektraKeyCompareName(const struct CKey *k1, const struct CKey *k2);

struct CKey *elektraKeyCopy(struct CKey *dest, const struct CKey *source, elektraCopyFlags flags);

int elektraKeyDel(struct CKey *key);

/**
 * You have to free the returned string manually, otherwise there will be memory leaks
 */
const char *elektraKeyEscapedName(const struct CKey *key);

ssize_t elektraKeyEscapedNameSize(const struct CKey *key);

int elektraKeyIsBelow(struct CKey *key, struct CKey *check);

int elektraKeyIsBelowOrSame(struct CKey *key, struct CKey *check);

int elektraKeyIsDirectlyBelow(const struct CKey *key, const struct CKey *check);

int elektraKeyIsLocked(const struct CKey *key, elektraLockFlags what);

int elektraKeyLock(struct CKey *key, elektraLockFlags what);

struct CKeySet *elektraKeyMeta(struct CKey *key);

const char *elektraKeyName(const struct CKey *key);

ssize_t elektraKeyNameSize(const struct CKey *key);

enum elektraNamespace elektraKeyNamespace(const struct CKey *key);

struct CKey *elektraKeyNew(const char *keyname);

ssize_t elektraKeySetBaseName(struct CKey *key, const char *baseName);

int elektraKeySetMeta(struct CKey *key, struct CKeySet *meta);

ssize_t elektraKeySetName(struct CKey *key, const char *newname);

ssize_t elektraKeySetNamespace(struct CKey *key, enum elektraNamespace ns);

ssize_t elektraKeySetValue(struct CKey *key, const void *value, size_t valueSize);

const void *elektraKeyValue(const struct CKey *key);

ssize_t elektraKeyValueSize(const struct CKey *key);

ssize_t elektraKeysetAdd(struct CKeySet *ks, struct CKey *key);

ssize_t elektraKeysetAddAll(struct CKeySet *ks, const struct CKeySet *other);

int elektraKeysetClear(struct CKeySet *ks);

uint16_t elektraKeysetDecRef(struct CKeySet *ks);

int elektraKeysetDel(struct CKeySet *ks);

struct CKey *elektraKeysetGet(const struct CKeySet *ks, ssize_t index);

uint16_t elektraKeysetGetRef(const struct CKeySet *ks);

uint16_t elektraKeysetIncRef(struct CKeySet *ks);

struct CKey *elektraKeysetLookup(const struct CKeySet *ks, struct CKey *key);

struct CKey *elektraKeysetLookupByName(const struct CKeySet *ks, const char *name);

struct CKeySet *elektraKeysetNew(size_t alloc);

struct CKey *elektraKeysetRemove(struct CKeySet *ks, ssize_t index);

ssize_t elektraKeysetSearch(const struct CKeySet *ks, const struct CKey *k);

ssize_t elektraKeysetSize(const struct CKeySet *ks);


int main (void) {
    CKey * key = elektraKeyNew ("user:/test/qwe/asd");
    printf("%s\n", elektraKeyName (key));

    CKey * key2 = elektraKeyNew ("user:/test/qwe/asd/qwe");
    printf("%s\n", elektraKeyName (key2));

    printf("--------------\n");
    printf("KeyIsBelow:\n");

    printf("%i\n", elektraKeyIsBelow (key, key2));
    printf("%i\n", elektraKeyIsBelow (key2, key));

    printf("--------------\n");
    printf("keyAddName:\n");

    printf("%zu\n", elektraKeyAddName (key, "yyyyyyy"));
    printf("%s\n", elektraKeyName (key));
    printf("%s\n", elektraKeyBaseName (key));
    printf("%zu\n", elektraKeyBaseNameSize (key));

    printf("--------------\n");
    printf("keySetName:\n");

    printf("%zu\n", elektraKeySetName (key, "user:/asd/qwe/asd"));
    printf("%s\n", elektraKeyName (key));
    printf("%s\n", elektraKeyBaseName (key));
    printf("%zu\n", elektraKeyBaseNameSize (key));

    printf("--------------\n");
    printf("keySetBaseName:\n");

    printf("%zu\n", elektraKeySetBaseName (key, ""));
    printf("%s\n", elektraKeyName (key));
    printf("%s\n", elektraKeyBaseName (key));
    printf("%zu\n", elektraKeyBaseNameSize (key));

    printf("--------------\n");
    printf("keyClear:\n");

    elektraKeyClear (key);
    printf("%s\n", elektraKeyName (key));

    printf("--------------\n");
    printf("keyValue:\n");

    const char * value = elektraKeyValue (key);
    printf("%p\n", value);

    printf("keySetValue:\n");
    printf("%zu\n", elektraKeySetValue(key, "abcd", 5));
    value = elektraKeyValue (key);
    printf("%p\n", value);
    printf("%s\n", value);

    printf("keySetValue:\n");
    printf("%zu\n", elektraKeySetValue(key, "abcd", 5));
    value = elektraKeyValue (key);
    printf("%p\n", value);
    printf("%s\n", value);

    printf("--------------\n");
    printf("keySet:\n");

    CKeySet * ks = elektraKeysetNew (1);
    printf("%zu\n", elektraKeysetSize (ks));
    printf("%i\n", elektraKeysetIncRef (ks));
    printf("%i\n", elektraKeysetIncRef (ks));
    printf("%i\n", elektraKeysetIncRef (ks));
    printf("%i\n", elektraKeysetIncRef (ks));
    printf("%i\n", elektraKeysetIncRef (ks));
    printf("%zu\n", elektraKeysetSize (ks));

    printf("%zu\n", elektraKeysetAdd (ks, key));
    printf("%zu\n", elektraKeysetSize (ks));

    printf("%zu\n", elektraKeysetAdd (ks, key2));
    printf("%zu\n", elektraKeysetSize (ks));

    printf("%s\n", elektraKeyName (elektraKeysetLookup (ks, key)));
    printf("%s\n", elektraKeyName (elektraKeysetLookup (ks, key2)));

    elektraKeysetDel (ks);

    elektraKeyDel (key);
    elektraKeyDel (key2);
}