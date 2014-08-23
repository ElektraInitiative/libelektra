#ifndef KDB_HELPER
#define KDB_HELPER

#ifdef __cplusplus
namespace ckdb {
extern "C" {
#endif

void *elektraMalloc (size_t size);
void *elektraCalloc (size_t size);
void  elektraFree (void *ptr);
char *elektraStrDup (const char *s);
char *elektraStrNDup (const char *s, size_t l);
int elektraRealloc(void **buffer, size_t size);

int elektraStrCmp (const char *s1, const char *s2);
int elektraStrCaseCmp (const char *s1, const char *s2);
size_t elektraStrLen(const char *s);

// not yet implemented:
int elektraKeyNameEscape(const char *source, char *dest);
int elektraKeyNameUnescape(const char *source, char *dest);

#ifdef __cplusplus
}
}
#endif

#endif
