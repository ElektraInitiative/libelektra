/**
 * @file
 *
 * @brief Operating system specific workarounds
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */


#ifndef KDBSTAT_H
#define KDBSTAT_H

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#if defined(__APPLE__)
#define elektraStatSeconds(status) status.st_mtime
#define elektraStatNanoSeconds(status) status.st_mtimespec.tv_nsec
#elif defined(_WIN32)
#define elektraStatSeconds(status) status.st_mtime
#define elektraStatNanoSeconds(status) 0
#else
#define elektraStatSeconds(status) status.st_mtim.tv_sec
#define elektraStatNanoSeconds(status) status.st_mtim.tv_nsec
#endif

#endif
