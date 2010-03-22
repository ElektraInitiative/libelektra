/**
  kdbloader.h

  Abstraction from loading symbols for different OS.

  A void* has been used, because not on every platform a
  struct is used. It does not give any type safety, but
  because the code within these functions is trivial anyway
  that should be no problem.
 */


#ifndef KDBLIBLOADER_H
#define KDBLIBLOADER_H

typedef void* kdbLibHandle;

/* General pointer to kdbLib Functions and pointer to kdbLibBackend function */
typedef void (*kdbLibFunc)(void);

/* Functions */
int kdbLibInit(void);
kdbLibHandle kdbLibLoad(const char *backendName);
kdbLibFunc kdbLibSym(kdbLibHandle handle, const char *symbol);
int kdbLibClose(kdbLibHandle handle);
const char *kdbLibError(void);

#endif /* KDBLIBLOADER_H */
