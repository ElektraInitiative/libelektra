#ifndef ELEKTRA_RESOLVER_STAT_H
#define ELEKTRA_RESOLVER_STAT_H

#if defined(__APPLE__)
#define ELEKTRA_STAT_SECONDS(status) status.st_mtime
#define ELEKTRA_STAT_NANO_SECONDS(status) status.st_mtimespec.tv_nsec
#elif defined(_WIN32)
#define ELEKTRA_STAT_SECONDS(status) status.st_mtime
#define ELEKTRA_STAT_NANO_SECONDS(status) 0
#else
#define ELEKTRA_STAT_SECONDS(status) status.st_mtim.tv_sec
#define ELEKTRA_STAT_NANO_SECONDS(status) status.st_mtim.tv_nsec
#endif

#endif // ELEKTRA_RESOLVER_STAT_H
