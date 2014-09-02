#ifndef LOCK_H
#define LOCK_H

#include <kdb.h>

int elektraLockFile(int fd, Key *parentKey);
int elektraUnlockFile(int fd, Key *parentKey);
void elektraCloseFile(int fd, Key *parentKey);

#endif
