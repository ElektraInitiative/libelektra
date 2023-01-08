# PREVIEW: New `libelektra-core` API 2023

## Core API

### Note on names

From the [use cases](/doc/usecases/core/README.md) we know that there is a collection structure which contains elements with a name and value (and metadata).

The current API uses these names for those concepts:

- Collection: `KeySet`
- Element: `Key`
- Name of element: `char *`
- Value of element: —

The API proposed below uses (to keep as much as possible from above):

- Collection: `ElektraKeyset`
- Element: `ElektraKey`
- Name of element: `ElektraKeyname`
- Value of element: `ElektraKeyvalue`

Another (maybe less confusing) option would be:

- Collection: `ElektraSet` or `ElektraMap`
- Element: `ElektraEntry`
- Name of element: `ElektraKey` or `ElektraName`
- Value of element: `ElektraValue`

### Note on public structs

The public structs `ElektraKeyname` and `ElektraKeyvalue` defined below are not problematic or limiting in terms of forward compatibility.
This is because, they essentially define what a "keyname" and a "key value" are for Elektra.
That is why they are used both as part of the public API **and** in the definitions of the `*Cow` structs, which define how stuff is actually stored.
Any change to these structs would mean changing that definition and therefore would be major breaking change, even if the structs where not public.

### Public headers

```c
// indicates special rules for constness, see comments at use-site for details
#define ELEKTRA_CONST const

typedef uint8_t ElektraNamespace;

typedef struct {
    // constness follows containing struct
    // if the containing struct is a `const ElektraKeyname *` then this field must not be modified
    // if the containing struct is a `ElektraKeyname *` or a non-pointer `ElektraKeyname` it is okay to modify
    ELEKTRA_CONST char * name;
    size_t size;
    ElektraNamespace ns;
} ElektraKeyname;

typedef struct {
    const void * value;
    size_t size;
} ElektraKeyvalue;

typedef struct ElektraKey ElektraKey;
typedef struct ElektraKeyset ElektraKeyset;

// 0 always indicates success, other values depend on function
typedef int ElektraErrorCode;

typedef enum {
    ELEKTRA_KEY_CP_NAME   = 1<<0,
    ELEKTRA_KEY_CP_VALUE  = 1<<1,
    ELEKTRA_KEY_CP_META   = 1<<2,
    ELEKTRA_KEY_CP_ALL    = ELEKTRA_KEY_CP_NAME | ELEKTRA_KEY_CP_VALUE | ELEKTRA_KEY_CP_META,
} ElektraKeyCopyFlag;

void elektraKeynamePushPart (ElektraKeyname * name, const char * part);
void elektraKeynamePopPart (ElektraKeyname * name);

ElektraKey * elektraKeyNew (const ElektraKeyname * name);
ElektraKey * elektraKeyRetain (const ElektraKey * key);
void elektraKeyRelease (const ElektraKey * key);

const ElektraKeyname * elektraKeyGetName (const ElektraKey * key);
// Note: creates copy of name
ElektraErrorCode elektraKeySetName (ElektraKey * key, const ElektraKeyname * name);
// increments nameLock
void elektraKeyLockName (ElektraKey * key);
// decrements nameLock
void elektraKeyUnlockName (ElektraKey * key);
bool elektraKeyIsNameLocked (const ElektraKey * key);

const ElektraKeyvalue * elektraKeyGetValue (const ElektraKey * key);
// Note: creates copy of value
ElektraErrorCode elektraKeySetValue (ElektraKey * key, const ElektraKeyvalue * value);

/**
 * @returns a `const ElektraKeyset *` that can safely be cast to `ElektraKeyset *`, iff a non-const `ElektraKey *` was passed as @p key
 */
ELEKTRA_CONST ElektraKeyset * elektraKeyGetMeta (const ElektraKey * key);

ElektraErrorCode elektraKeyIsBelow (const ElektraKey * first, const ElektraKey * second);
ElektraErrorCode elektraKeyCompare (const ElektraKey * first, const ElektraKey * second);

ElektraKey * elektraKeyCopy (ElektraKey * dest, const ElektraKey * src, ElektraKeyCopyFlag flags);
static inline ElektraKey * elektraKeyDup (ElektraKey * key, ElektraKeyCopyFlag flags)
{
    ElektraKey * dest = flags & ELEKTRA_KEY_CP_NAME
        ? elektraKeyNew (elektraKeyGetName (key))
        : elektraKeyNew (&(ElektraKeyname){.ns = ELEKTRA_NS_CASCADING, .name = "", .size = 0});
    return elektraKeyCopy (dest, key, (ElektraKeyCopyFlag)(flags & ~ELEKTRA_KEY_CP_NAME));
}


ElektraKeyset * elektraKeysetNew (size_t prealloc);
ElektraKeyset * elektraKeysetRetain (const ElektraKeyset * ks);
void elektraKeysetRelease (const ElektraKeyset * ks);

size_t elektraKeysetSize (ElektraKeyset * ks);

ElektraErrorCode elektraKeysetInsert (ElektraKeyset * ks, ElektraKey * key);
ElektraErrorCode elektraKeysetInsertAll (ElektraKeyset * ks, ElektraKeyset * other);
static inline ElektraErrorCode elektraKeysetInsertAndRelease (ElektraKeyset * ks, ElektraKey * key)
{
    ElektraErrorCode error = elektraKeysetInsert (ks, key);
    elektraKeyRelease (key);
    return error;
}

ElektraKey * elektraKeysetGet (ElektraKeyset * ks, size_t index);
ElektraKeyset * elektraKeysetGetRange (ElektraKeyset * ks, size_t start, size_t end);

void elektraKeysetRemove (ElektraKeyset * ks, size_t index);
void elektraKeysetRemoveRange (ElektraKeyset * ks, size_t start, size_t end);
static inline void elektraKeysetClear (ElektraKeyset * ks)
{
    elektraRemoveRange (ks, 0, elektraKeysetSize (ks));
}

size_t elektraKeysetLookup (ElektraKeyset * ks, const ElektraKeyname * name);

size_t elektraKeysetFindHierarchy (ElektraKeyset * ks, const ElektraKeyname * root, size_t * end);
```

### Private headers

```c
struct ElektraKeynameCow {
    struct ElektraKeyname name;

    uint16_t refs;

    bool shouldFree : 1;
    int reserved : 15;
};

struct ElektraKeyvalueCow {
    struct ElektraKeyvalue value;

    uint16_t refs;

    bool shouldFree : 1;
    int reserved : 15;
};

struct ElektraKeyset;

struct ElektraKey {
    struct ElektraKeynameCow * name;
    struct ElektraKeyvalueCow * value;
    struct ElektraKeyset * meta;

    uint16_t refs;
    uint16_t nameLock;

    bool shouldFree : 1;
    bool needsSync : 1;
    int reserved : 14;
};

struct ElektraKeyset {
    struct ElektraKeysetDataCow * data;

    uint16_t refs;

    bool shouldFree : 1; // used for mmap
    int reserved : 15;
};
```

### Usage Examples

```c
// Create key (Note: better ways below via additional APIs)
ElektraKey * k1 = elektraKeyNew (&(ElektraKeyname){
    .ns = ELEKTRA_NS_SYSTEM,
    .name = "foo\0bar\0baz",
    .size = 14
});

// get name from key
const ElektraKeyname * name1 = elektraKeyGetName (k1);

// create local name
ElektraKeyname name2 = {
    .ns = ELEKTRA_NS_USER,
    .name = "foo",
    .size = 4
};

// ... using existing name
ElektraKey * k2 = elektraKeyNew (&name2);

// manipulate name
elektraKeynamePushPart (&name2, "boo");
ElektraKey * k3 = elektraKeyNew (&name2);


// manipulate name from key (needs copy)
ElektraKeyname name1Copy = {
    .ns = name1->ns,
    .name = malloc (name1->size),
    .size = name1->size
};
memcpy (name1Copy.name, name1->size);
elektraKeynamePopPart (&name1Copy, "boo");
ElektraKey * k4 = elektraKeyNew (&name1Copy);

// cleanup name copy
free (name1Copy.name);
name1Copy.name = NULL;
name1Copy.size = 0;

// Create keyset and insert keys (Note: better ways below via additional APIs)
ElektraKeyset * ks1 = elektraKeysetNew (8);

elektraKeysetInsert (ks1, k1);            // creates new reference to k1 inside ks1
elektraKeysetInsertAndRelease (ks1, k2);  // moves reference from k2 into ks1, k2 must no longer be

elektraKeysetInsertAndRelease (ks1, k3);
elektraKeysetInsertAndRelease (ks1, k4);
elektraKeysetInsertAndRelease (ks1, k5);
elektraKeysetInsertAndRelease (ks1, k6);
elektraKeysetInsertAndRelease (ks1, k7);
elektraKeysetInsertAndRelease (ks1, k8);

// copy keys from one keyset (ks2) to another (ks1)
elektraKeysetInsertAll (ks1, ks2);

// give up reference to keyset and free memory if last reference
elektraKeysetRelease (ks2);
// we must now stop using ks2

// lookup key inside keyset, with name from existing key ...
ElektraKey * l1 = elektraKeysetLookup (ks1, elektraKeyGetName (k1));

// with local name ...
// Note: Using elektraKeyGetName (k2) would also be unsafe, because we've given up that reference
ElektraKey * l2 = elektraKeyRetain (elektraKeysetLookup (ks1, name2));
// or inline name
ElektraKey * l3 = elektraKeysetLookup (ks1, &{ .ns = ELEKTRA_NS_CASCADING, .name = "foo", .size = 4 });
// cascading lookup returns same key
assert (l2 == l3);

// give up reference to key
elektraKeyRelease (k1);
// we must now stop using k1 & name1 (because name1 is tied to k1)
// using l1 & l3 is still okay, because that reference is tied to ks1, until we elektraKeysetRelease (ks1), we can still use l1 & l3
// using l2 is fine for even longer, because we called elektraKeyRetain, we retained our own reference
// until we call both elektraKeysetRelease (ks1) AND elektraKeyRelease (l2), we can still use l2

// give up additional reference to key
elektraKeyRelease (l2);
// we can still use l2, but the lifetime is now tied to ks1 again

// create new reference to key
elektraKeyRetain (l3);

// give up reference to keyset
elektraKeysetRelease (ks1);
// we must now stop using l1 & l2
// we can still use l3, because we retained our own reference
```

For completeness, here are some more example of unsafe and illegal operations.
These operations compile and "work" without reporting errors at runtime, but will cause various problems.
An "illegal" operation, immediately break something and will cause wrong results, segfaults, etc. down the line.
An "unsafe" operation, does not immediately break anything, but it is still an incorrect use of the API and often creates new "unsafe" or even "illegal" operations for the future.

```c
ElektraKey * k1 = elektraKeyNew (&(ElektraKeyname){
    .ns = ELEKTRA_NS_SYSTEM,
    .name = "foo\0bar\0baz",
    .size = 14
});
ElektraKeyset * ks1 = elektraKeysetNew (8);
elektraKeyInsertAndRelease (ks1, k1);

ElektraKey * i1 = elektraKeysetGet (ks1, 0);

// WARNING USAFE OPERATIONS
elektraKeyUnlockName (i1); // name can now be modified even though k1 is still part of ks1

// WARNING ILLEGAL OPERATIONS

// because of the elektraKeyUnlockName, this is now an ILLEGAL operation, we are changing the name of a key that is part of a keyset
// without elektraKeyUnlockName it would be safe, and elektraKeySetName would return an error
elektraKeySetName (i1, name2);


// having too many elektraKeyRelease is also an IILEGAL operation
// here we've already given up our references to these keys, so calling elektraKeyRelease again means we're releasing somebody else's reference
// in this case this will even lead to the memory of k1 being freed and ks1 containing invalid pointers, because we're releasing the reference to k1 that is held by ks1
elektraKeyRelease (k1);
```

## API outside `libelektra-core`

### Names

```c
void elektraKeynameAppend (ElektraKeyname * name, const char * suffix, size_t size);

// constness of return value follows constness of name
ELEKTRA_CONST char * elektraKeynameLastPart (const ElektraKeyname * name, size_t * outSize);
void elektraKeynameReplaceLastPart (ElektraKeyname * name, const char * newPart);

// constness of return value follows constness of name
ELEKTRA_CONST char * elektraKeynameNextPart (const ElektraKeyname * name, const char * currentPart);

/**
 * allocates a new string with the escaped form of @p name
 */
char * elektraKeynameEscape (const ElektraKeyname * name);

/**
 * Verifies that @p escaped is a valid escaped name, and optionally produces
 *   (1) the canonical escaped form
 *   (2) the size of the canonical escaped form
 *   (3) the unescaped form
 *   (4) a pointer to the first error in @p escaped
 *
 * @param dest If != NULL, will contain newly allocated unescaped form on success
 * @param escaped Escaped name to process (NUL-terminated string)
 * @param canonial If != NULL, will be newly allocated string (NUL-terminated) with canonical form of @p escaped
 * @param canonicalSize If != NULL, will be the length of the string written to @p canonical (including NUL-terminator)
 * @param errorPtr If != NULL, will be a pointer into @p escaped, which points to the location of the first error, i.e. the error which is described by the return value
 *
 * @returns 0 if @p escaped is a valid escaped name (other values for different kinds of errors)
 */
ElektraErrorCode elektraKeynameProcessEscaped (
    ElektraKeyname * dest,
    const char * escaped,
    char ** canonical,
    size_t * canonicalSize,
    const char ** errorPtr
);
```

#### Usage Examples

```c
// just check if valid, and print error
const char * errorLoc;
ElektraErrorCode error1 = elektraKeynameProcessEscaped (NULL, "system:foo", NULL, NULL, &errorLoc);
if (error1 != 0)
{
    // prints e.g.:
    // Name invalid, error 1: :foo
    printf ("Name invalid, error %d: %s\n", error1, errorLoc);
}

// find canonical form and size
char * canonical;
size_t csize;
ElektraErrorCode error2 = elektraKeynameProcessEscaped (NULL, "system://foo/#123", &canonical, &csize, NULL);
if (error2 == 0)
{
    // prints:
    // Canonical size: 19
    // Canonical name: system:/foo/#__123
    printf ("Canonical size: %zd\n", csize);
    printf ("Canonical name: %s\n", canonical);
}
else
{
    printf ("error: %d\n", error2);
}

// produce unescaped form and create key
ElektraKeyname name;
ElektraErrorCode error3 = elektraKeynameProcessEscaped (&name, "system://foo/#123", NULL, NULL, NULL);
if (error3 != 0)
{
    printf ("error: %d\n", error3);
    exit (1);
}

ElektraKey * key = elektraKeyNew (&name);
elektraKeynameFree (&name);

elektraKeyRelease (key);
```

### Cut

```c
// cuts single key out of keyset
ElektraKey * elektraKeysetCut (ElektraKeyset * ks, size_t index)
{
    ElektraKey * key = elektraKeysetGet (ks, index);
    elektraKeysetRemove (ks, index);
    return key;
}

// Migration Note: replacement for ksLookup(ks, key, KDB_O_POP) would be
/*
elektraKeysetCut (ks, elektraKeysetLookup (ks, elektraKeyGetName (key)));
*/

// cuts arbitrary range out of keyset
ElektraKeyset * elektraKeysetCutRange (ElektraKeyset * ks, size_t start, size_t end)
{
    ElektraKeyset * range = elektraKeysetGetRange (ks, start, end);
    elektraKeysetRemoveRange (ks, start, end);
    return range;
}

// cuts hierarchy out of keyset
// Migration Note: replaces current ksCut
ElektraKeyset * elektraKeysetCutHierarchy (ElektraKeyset * ks, const ElektraKeyname * root)
{
    size_t end;
    size_t start = elektraKeysetFindHierarchy (ks, root, &end);
    return elektraKeysetCutRange (ks, start, end);
}
```

### Builders

```c
#define ELEKTRA_KEYSET(...) (elektraKeysetBuild ((ElektraKey *[]){__VA_ARGS__}, sizeof((ElektraKey *[]){__VA_ARGS__})/sizeof(ElektraKey*)))

#define ELEKTRA_KEYNAME(ns_, name_) (ElektraKeyname){.ns = (ns_), .name = (name_), .size = sizeof (name_)}
#define ELEKTRA_METANAME(name) ELEKTRA_KEYNAME(ELEKTRA_NS_META, (name))
#define ELEKTRA_KEYVALUE_STRING(s) (ElektraKeyvalue){.value = (s), .size = strlen((s))}
#define ELEKTRA_KEYVALUE_PTR(v) (ElektraKeyvalue){.value = &(v), .size = sizeof((v))}

/**
 * CAUTION releases reference to @p meta
 */
ElektraKey * elektraKeyBuild (const ElektraKeyname * name, const ElektraKeyvalue * value, ElektraKeyset * meta)
{
    ElektraKey * key = elektraKeyNew (name);
    elektraKeySetValue (key, value);
    elektraKeysetInsertAll ((ElektraKeyset *)elektraKeyGetMeta (key), meta);
    elektraKeysetRelease (meta);
    return key;
}

ElektraKey * elektraKeyBuildMeta (const ElektraKeyname * name, const ElektraKeyvalue * value)
{
    if (name->ns != ELEKTRA_NS_META)
    {
        return NULL;
    }
    return elektraKeyBuild (name, value, NULL);
}

/**
 * DOES NOT retain new references to @p keys, instead moves existing reference into returned keyset
 */
ElektraKeyset * elektraKeysetBuild (ElektraKey ** keys, size_t size)
{
    ElektraKeyset * ks = elektraKeysetNew (size);

    // build fake ElektraKeyset with everything that elektraKeysetInsertAll needs
    // depending on elektraKeysetInsertAll this may require copying/sorting keys
    ElektraKeyset fake = {
        .data = &(struct ElektraKeysetDataCow) {
            .array = keys,
            .size = size,
        }
    };
    elektraKeysetInsertAll (ks, &fake);

    return ks;
}
```

#### Usage Examples

```c
int a = 7;

ElektraKey * key = elektraKeyNew (&ELEKTRA_KEYNAME(ELEKTRA_NS_SYSTEM, "foo")); // refs = 1

ElektraKeyset * ks = ELEKTRA_KEYSET (
    // elektraKeysetBuild consumes reference, doesn't retain itself
    // we call elektraKeyRetain, so we can keep our own reference
    elektraKeyRetain (key), // refs = 2
    elektraKeyBuild (&ELEKTRA_KEYNAME(ELEKTRA_NS_SYSTEM, "foo\0bar"), NULL, NULL),
    elektraKeyBuild (&ELEKTRA_KEYNAME(ELEKTRA_NS_USER, "foo\0bar"), &ELEKTRA_KEYVALUE_STRING("hello"), NULL),
    elektraKeyBuild (&ELEKTRA_KEYNAME(ELEKTRA_NS_PROC, "foo\0bar"), &ELEKTRA_KEYVALUE_PTR (a), ELEKTRA_KEYSET (
        elektraKeyBuildMeta (&ELEKTRA_METANAME("type"), &ELEKTRA_KEYVALUE_STRING("long")),
        elektraKeyBuildMeta (&ELEKTRA_METANAME("check\0min"), &ELEKTRA_KEYVALUE_STRING("4")),
    )),
    elektraKeyBuild (&ELEKTRA_KEYNAME(ELEKTRA_NS_SYSTEM, "foo\0bar\0abc"), NULL, NULL)
); // refs = 2, rest in ks has refs = 1

elektraKeysetRelease (ks); // refs = 1, rest of ks deleted

elektraKeyRelease (key); // refs = 0, key deleted
```
