// The following ifdef block is the standard way of creating macros which make exporting 
// from a DLL simpler. All files within this DLL are compiled with the WINREGISTRY_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see 
// WINREGISTRY_API functions as being imported from a DLL, whereas this DLL sees symbols
// defined with this macro as being exported.
#ifdef WINREGISTRY_EXPORTS
#define WINREGISTRY_API __declspec(dllexport)
#else
#define WINREGISTRY_API __declspec(dllimport)
#endif
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include <kdb.h>
#include <kdbbackend.h>

#define BACKENDNAME "winregistry"
#define KDB_REGISTRY_PATH "SOFTWARE/Elektra"

/* Private functions */
size_t kdbGetRegistryPath(const Key *forKey,HKEY rootkey, char *path, size_t maxSize, char **keyName);
size_t getSID(char *username, char *sid, int size);

/* Exported functions */
#ifdef __cplusplus
extern "C" {
#endif
WINREGISTRY_API int kdbOpen_winregistry();
WINREGISTRY_API int kdbClose_winregistry();
WINREGISTRY_API int kdbStatKey_winregistry(Key *key);
WINREGISTRY_API int kdbGetKey_winregistry(Key *key);
WINREGISTRY_API int kdbSetKey_winregistry(Key *key);
WINREGISTRY_API int kdbRename_winregistry(Key *key, const char *newName);
WINREGISTRY_API int kdbRemoveKey_winregistry(const Key *key);
WINREGISTRY_API ssize_t kdbGetKeyChildKeys_winregistry(const Key *parentKey, KeySet *returned, unsigned long options);

/* While the windows registry does provide a far more efficient implementation of these, it's fine to live without them for now */
/*int kdbSetKeys_winregistry(KeySet *ks);
uint32_t kdbMonitorKeys_winregistry(KeySet *interests, uint32_t diffMask, unsigned long iterations, unsigned sleep);
uint32_t kdbMonitorKey_winregistry(Key *interest, uint32_t diffMask, unsigned long iterations, unsigned sleep);*/

/**
 * All KeyDB methods implemented by the backend can have random names, except
 * kdbBackendFactory(). This is the single symbol that will be looked up
 * when loading the backend, and the first method of the backend
 * implementation that will be called.
 * 
 * Its purpose is to "publish" the exported methods for libelektra.so. The
 * implementation inside the provided skeleton is usually enough: simply
 * call kdbBackendExport() with all methods that must be exported.
 * 
 * @return whatever kdbBackendExport() returns
 * @see kdbBackendExport() for an example
 * @see kdbOpenBackend()
 * @ingroup backend
 */
WINREGISTRY_API KDBBackend *kdbBackendFactory(void);

#ifdef __cplusplus
}
#endif
