#define _GNU_SOURCE
#include <dlfcn.h>
#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/kdb/kdb.h>
#include <fcntl.h>
#include <libgen.h>
#include <limits.h>
#include <linux/limits.h>
#include <pwd.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#in.h>
clude<sys / time.h>
#include <sys/#include <elektra/core/key.h>
		types.h >
#include <unistd.h>

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/kdb/kdb.h>
#include <internal/kdbprivate.h>
#include <internal/pluginload/module.h>

#define PRELOAD_PATH "/elektra/intercept/open"
#define TV_MAX_DIGITS 26
#define RELEVANT_FRAME 1

	struct _Node
{
	char * key;
	char * value;
	unsigned short oflags;
	char * exportType;
	char * exportKey;
	time_t creationTime;
	struct _Node * next;
};
typedef struct _Node Node;
static Node * head = NULL;

static void canonicalizePath (char * buffer, char * toAppend)
{
	char * destPtr = buffer + strlen (buffer);
	for (unsigned int i = 0; i < strlen (toAppend); ++i)
	{
		if (!strncmp ((toAppend + i), "../", 3))
		{
			i += 2;
			const char * dir = dirname (buffer);
			size_t dirLen = strlen (dir);
			destPtr = buffer + dirLen;
			if (strcmp (dir, "/")) *destPtr++ = '/';
			*destPtr = '\0';
			continue;
		}
		else if (!strncmp ((toAppend + i), "./", 2))
		{
			++i;
			continue;
		}
		else if (!strncmp ((toAppend + i), "//", 2))
		{
			continue;
		}
		else
		{
			*destPtr++ = toAppend[i];
		}
	}
}

static char * createAbsolutePath (const char * path, const char * cwd)
{
	if (path[0] == '/')
		return elektraStrDup (path);
	else
	{
		char * absPath = NULL;
		size_t pathlen;
		char * pathPtr = NULL;
		if (path[0] == '~')
		{
			struct passwd * pwd = getpwuid (getuid ());
			pathlen = strlen (path) + strlen (pwd->pw_dir) + 2;
			absPath = calloc (pathlen, sizeof (char));
			snprintf (absPath, pathlen, "%s/", pwd->pw_dir);
			pathPtr = (char *) (path + 2);
		}
		else
		{
			pathlen = strlen (path) + strlen (cwd) + 2;
			absPath = calloc (pathlen, sizeof (char));
			snprintf (absPath, pathlen, "%s/", cwd);
			pathPtr = (char *) path;
		}
		canonicalizePath (absPath, pathPtr);
		return absPath;
	}
}

static const char * genTemporaryFilename (void)
{
	struct timeval tv;
	gettimeofday (&tv, 0);
	const char * fileName = "/tmp/.elektra_generated";
	size_t len = strlen (fileName) + TV_MAX_DIGITS + 1;
	char * tmpFile = elektraCalloc (len);
	snprintf (tmpFile, len, "%s_%lu:%lu", fileName, tv.tv_sec, tv.tv_usec);
	return tmpFile;
}

static void init (void) __attribute__ ((constructor));
static void cleanup (void) __attribute__ ((destructor));
void init (void)
{
	char cwd[KDB_MAX_PATH_LENGTH];
	getcwd (cwd, KDB_MAX_PATH_LENGTH);
	KeySet * tmpKS = ksNew (0, KS_END);
	Key * parentKey = keyNew (PRELOAD_PATH, KEY_END);
	KDB * handle = kdbOpen (NULL, parentKey);
	kdbGet (handle, tmpKS, parentKey);
	KeySet * ks = ksCut (tmpKS, parentKey);
	ssize_t size = ksGetSize (ks);
	if (size <= 1) goto CleanUp;
	Node * current = head;


	for (elektraCursor it = 1; it < ksGetSize (ks); ++it) // skip head
	{
		const Key * key = ksAtCursor (ks, it);
		if (!keyIsDirectlyBelow (parentKey, key)) continue;
		Node * tmp = calloc (1, sizeof (Node));
		tmp->key = createAbsolutePath (keyBaseName (key), cwd);
		if (!strcmp (keyString (key), ""))
			tmp->value = NULL;
		else
			tmp->value = createAbsolutePath (keyString (key), cwd);
		tmp->oflags = (unsigned short) -1;
		Key * lookupKey = keyDup (key, KEY_CP_ALL);
		keyAddBaseName (lookupKey, "readonly");
		Key * found = ksLookup (ks, lookupKey, 0);
		if (found)
		{
			it = ksSearch (ks, found) + 1; // ksLookup sets internal iterator at found key
			if (!strcmp (keyString (found), "1"))
			{
				tmp->oflags = O_RDONLY;
			}
		}
		keySetBaseName (lookupKey, 0);
		keyAddBaseName (lookupKey, "generate");
		found = ksLookup (ks, lookupKey, 0);
		if (found)
		{
			it = ksSearch (ks, found) + 1; // ksLookup sets internal iterator at found key
			if (tmp->value == NULL) tmp->value = (char *) genTemporaryFilename ();
			tmp->exportKey = elektraStrDup (keyString (found));
			keyAddBaseName (lookupKey, "plugin");
			found = ksLookup (ks, lookupKey, 0);
			if (found)
			{
				it = ksSearch (ks, found) + 1; // ksLookup sets internal iterator at found key
				tmp->exportType = elektraStrDup (keyString (found));
			}
			else
			{
				tmp->exportKey = NULL;
				tmp->exportType = NULL;
			}
		}
		else
		{
			tmp->exportKey = NULL;
			tmp->exportType = NULL;
		}
		keyDel (lookupKey);
		if (tmp->value == NULL) tmp->value = createAbsolutePath (keyBaseName (key), cwd);
		tmp->creationTime = 0;
		tmp->next = NULL;
		if (current == NULL)
		{
			head = tmp;
			current = head;
		}
		else
		{
			current->next = tmp;
			current = current->next;
		}
	}
CleanUp:
	ksAppend (tmpKS, ks);
	ksDel (tmpKS);
	ksDel (ks);
	kdbClose (handle, parentKey);
	keyDel (parentKey);
}


void cleanup (void)
{
	Node * current = head;
	while (current)
	{
		Node * tmp = current;
		free (current->key);
		if (current->value) free (current->value);
		if (current->exportKey)
		{
			free (current->exportKey);
			free (current->exportType);
		}
		current = current->next;
		free (tmp);
	}
}

static Node * resolvePathname (const char * pathname)
{
	Node * node = NULL;
	if (pathname)
	{
		char cwd[KDB_MAX_PATH_LENGTH];
		getcwd (cwd, KDB_MAX_PATH_LENGTH);
		char * resolvedPath = NULL;
		if (pathname[0] != '/')
		{
			resolvedPath = createAbsolutePath (pathname, cwd);
		}
		else
		{
			resolvedPath = calloc (strlen (pathname) + 1, sizeof (char));
			size_t size = sizeof (resolvedPath);
			memset (resolvedPath, 0, size);
			canonicalizePath (resolvedPath, (char *) pathname);
		}
		Node * current = head;
		while (current)
		{
			if (!strcmp (current->key, resolvedPath))
			{
				node = current;
				break;
			}
			current = current->next;
		}
		free (resolvedPath);
	}
	return node;
}

int __xstat (int ver, const char * path, struct stat * buf);
int __xstat64 (int ver, const char * path, struct stat64 * buf);

static void exportConfiguration (Node * node)
{
	Key * key = keyNew (node->exportKey, KEY_END);
	KDB * handle = kdbOpen (NULL, key);
	KeySet * ks = ksNew (0, KS_END);
	kdbGet (handle, ks, key);
	KeySet * exportKS;
	exportKS = ksCut (ks, key);
	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);
	KeySet * conf = ksNew (0, KS_END);
	Plugin * check = elektraPluginOpen (node->exportType, modules, conf, key);
	keySetString (key, node->value);
	check->kdbSet (check, exportKS, key);
	ksDel (conf);
	ksAppend (ks, exportKS);
	ksDel (exportKS);
	elektraModulesClose (modules, 0);
	ksDel (modules);
	keyDel (key);
	ksDel (ks);
	kdbClose (handle, 0);
	struct stat buf;
	if (!__xstat (3, node->value, &buf)) node->creationTime = buf.st_mtim.tv_sec;
}


// e.g. we intercept successive calls of stat and open
static inline int createdWithinTimeframe (int (*f) (int, const char *, struct stat *), Node * node, int frame)
{
	struct stat buf;
	if (!f (3, node->value, &buf))
	{
		if (node->creationTime && ((node->creationTime + frame) < buf.st_mtim.tv_sec)) return 0;
	}
	else
	{
		return 0;
	}
	return 1;
}

typedef int (*orig_open_f_type) (const char * pathname, int flags, ...);

typedef union
{
	void * d;
	orig_open_f_type f;
} OpenSymbol;


int open (const char * pathname, int flags, ...)
{
	Node * node = resolvePathname (pathname);
	const char * newPath = NULL;
	unsigned short newFlags = (unsigned short) -1;
	if (!node)
	{
		newPath = pathname;
	}
	else
	{
		if (!(node->exportType))
		{
			newPath = node->value;
			newFlags = node->oflags;
		}
		else
		{
			newPath = node->value;
			if (!createdWithinTimeframe (__xstat, node, RELEVANT_FRAME)) exportConfiguration (node);
		}
	}
	if (newFlags == O_RDONLY)
	{
		flags = (flags & (~(0 | O_WRONLY | O_APPEND)));
	}
	OpenSymbol orig_open;
	orig_open.d = dlsym (RTLD_NEXT, "open");

	int fd;
	if (flags & O_CREAT)
	{
		int mode;
		va_list argptr;
		va_start (argptr, flags);
		mode = va_arg (argptr, int);
		va_end (argptr);
		fd = orig_open.f (newPath, flags, mode);
	}
	else
	{
		fd = orig_open.f (newPath, flags);
	}
	return fd;
}
int open64 (const char * pathname, int flags, ...)
{
	Node * node = resolvePathname (pathname);
	const char * newPath = NULL;
	unsigned short newFlags = (unsigned short) -1;
	if (!node)
	{
		newPath = pathname;
	}
	else
	{
		if (!(node->exportType))
		{
			newPath = node->value;
			newFlags = node->oflags;
		}
		else
		{
			newPath = node->value;
			if (!createdWithinTimeframe (__xstat, node, RELEVANT_FRAME)) exportConfiguration (node);
		}
	}
	if (newFlags == O_RDONLY)
	{
		flags = (flags & (~(0 | O_WRONLY | O_APPEND)));
	}

	OpenSymbol orig_open64;
	orig_open64.d = dlsym (RTLD_NEXT, "open64");

	int fd;
	if (flags & O_CREAT)
	{
		int mode;
		va_list argptr;
		va_start (argptr, flags);
		mode = va_arg (argptr, int);
		va_end (argptr);
		fd = orig_open64.f (newPath, flags, mode);
	}
	else
	{
		fd = orig_open64.f (newPath, flags);
	}
	return fd;
}

typedef int (*orig_xstat_f_type) (int ver, const char * path, struct stat * buf);
typedef int (*orig_xstat64_f_type) (int ver, const char * path, struct stat64 * buf);

typedef union
{
	void * d;
	orig_xstat_f_type f;
} XstatSymbol;

typedef union
{
	void * d;
	orig_xstat64_f_type f;
} Xstat64Symbol;

int __xstat (int ver, const char * path, struct stat * buf)
{
	Node * node = resolvePathname (path);
	const char * newPath = NULL;
	XstatSymbol orig_xstat;
	orig_xstat.d = dlsym (RTLD_NEXT, "__xstat");
	if (!node)
		newPath = path;
	else
	{
		if (!(node->exportType))
		{
			newPath = node->value;
		}
		else
		{
			newPath = node->value;
			if (!createdWithinTimeframe (orig_xstat.f, node, RELEVANT_FRAME)) exportConfiguration (node);
		}
	}

	return orig_xstat.f (ver, newPath, buf);
}

int __xstat64 (int ver, const char * path, struct stat64 * buf)
{
	Node * node = resolvePathname (path);
	const char * newPath = NULL;
	Xstat64Symbol orig_xstat64;
	orig_xstat64.d = dlsym (RTLD_NEXT, "__xstat64");
	if (!node)
		newPath = path;
	else
	{
		if (!(node->exportType))
		{
			newPath = node->value;
		}
		else
		{
			newPath = node->value;
			if (!createdWithinTimeframe (__xstat, node, RELEVANT_FRAME)) exportConfiguration (node);
		}
	}

	return orig_xstat64.f (ver, newPath, buf);
}

typedef int (*orig_access_f_type) (const char * pathname, int mode);

typedef union
{
	void * d;
	orig_access_f_type f;
} AccessSymbol;

int access (const char * pathname, int mode)
{
	Node * node = resolvePathname (pathname);
	if (node && mode == F_OK) return 0;
	AccessSymbol orig_access;
	orig_access.d = dlsym (RTLD_NEXT, "access");
	return orig_access.f (pathname, mode);
}
