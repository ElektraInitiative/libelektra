#define _GNU_SOURCE
#include <dlfcn.h>
#include <fcntl.h>
#include <kdb.h>
#include <libgen.h>
#include <limits.h>
#include <linux/limits.h>
#include <pwd.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include <kdbmodule.h>
#include <kdbprivate.h>

#define PRELOAD_PATH "/preload/open"
#define TV_MAX_DIGITS 26

struct _Node
{
	char * key;
	char * value;
	unsigned short oflags;
	char * exportType;
	char * exportPath;
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
		return strdup (path);
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
			pathPtr = (char *)(path + 2);
		}
		else
		{
			pathlen = strlen (path) + strlen (cwd) + 2;
			absPath = calloc (pathlen, sizeof (char));
			snprintf (absPath, pathlen, "%s/", cwd);
			pathPtr = (char *)path;
		}
		canonicalizePath (absPath, pathPtr);
		return absPath;
	}
}

static void init (void) __attribute__ ((constructor));
static void cleanup (void) __attribute__ ((destructor));
void init ()
{
	char cwd[PATH_MAX];
	getcwd (cwd, PATH_MAX);
	KeySet * tmpKS = ksNew (0, KS_END);
	Key * parentKey = keyNew (PRELOAD_PATH, KEY_CASCADING_NAME, KEY_END);
	Key * key;
	KDB * handle = kdbOpen (parentKey);
	kdbGet (handle, tmpKS, parentKey);
	KeySet * ks = ksCut (tmpKS, parentKey);
	ksRewind (ks);
	ssize_t size = ksGetSize (ks);
	if (size <= 1) goto CleanUp;
	Node * current = head;
	ksNext (ks); // skip head
	while ((key = ksNext (ks)) != NULL)
	{
		Node * tmp = calloc (1, sizeof (Node));
		tmp->key = createAbsolutePath (keyBaseName (key), cwd);
		const Key * meta = keyGetMeta (key, "open/mode");
		if (meta)
		{
			if (!strcmp (keyString (meta), "ro"))
				tmp->oflags = O_RDONLY;
			else
				tmp->oflags = 0;
		}
		else
			tmp->oflags = 0;
		meta = keyGetMeta (key, "open/create");
		if (meta)
		{
			tmp->exportType = strdup (keyString (meta));
			tmp->value = strdup (keyString (key));
		}
		else
		{
			tmp->exportType = NULL;
			tmp->value = createAbsolutePath (keyString (key), cwd);
		}
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


void cleanup ()
{
	Node * current = head;
	while (current)
	{
		Node * tmp = current;
		free (current->key);
		free (current->value);
		if (current->exportType) free (current->exportType);
		if (current->exportPath) free (current->exportPath);
		current = current->next;
		free (tmp);
	}
}

static Node * resolvePathname (const char * pathname)
{
	Node * node = NULL;
	if (pathname)
	{
		char cwd[PATH_MAX];
		getcwd (cwd, PATH_MAX);
		char * resolvedPath = NULL;
		if (pathname[0] != '/')
		{
			resolvedPath = createAbsolutePath (pathname, cwd);
		}
		else
		{
			resolvedPath = calloc (strlen (pathname), sizeof (char));
			size_t size = sizeof(resolvedPath);
			memset (resolvedPath, 0, size);
			canonicalizePath (resolvedPath, (char *)pathname);
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

static void exportConfiguration (const char * pathname, Node * node)
{
	Key * key = keyNew (node->value, KEY_END);
	KDB * handle = kdbOpen (key);
	KeySet * ks = ksNew (0, KS_END);
	kdbGet (handle, ks, key);
	KeySet * exportKS;
	exportKS = ksCut (ks, key);
	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);
	KeySet * conf = ksNew (0, KS_END);
	Plugin * check = elektraPluginOpen (node->exportType, modules, conf, key);
	keySetString (key, pathname);
	ksRewind (exportKS);
	check->kdbSet (check, exportKS, key);
	ksDel (conf);
	ksAppend (ks, exportKS);
	ksDel (exportKS);
	elektraModulesClose (modules, 0);
	keyDel (key);
	ksDel (ks);
	kdbClose (handle, 0);
}


typedef int (*orig_open_f_type) (const char * pathname, int flags, ...);

int open (const char * pathname, int flags, ...)
{
	Node * node = resolvePathname (pathname);
	const char * newPath = NULL;
	unsigned short newFlags = (unsigned short)-1;
	if (!node)
		newPath = pathname;
	else
	{
		if (!(node->exportType))
		{
			newPath = node->value;
			newFlags = node->oflags;
		}
		else
		{
			node->exportPath = (char *)genTemporaryFilename ();
			newPath = node->exportPath;
			exportConfiguration (newPath, node);
		}
	}
	if (newFlags == O_RDONLY)
	{
		flags = (flags & (~(0 | O_WRONLY | O_APPEND)));
	}

	orig_open_f_type orig_open;
	orig_open = (orig_open_f_type)dlsym (RTLD_NEXT, "open");

	int fd;
	if (flags & O_CREAT)
	{
		int mode;
		va_list argptr;
		va_start (argptr, flags);
		mode = va_arg (argptr, int);
		va_end (argptr);
		fd = orig_open (newPath, flags, mode);
	}
	else
	{
		fd = orig_open (newPath, flags);
	}
	return fd;
}
int open64 (const char * pathname, int flags, ...)
{
	Node * node = resolvePathname (pathname);
	const char * newPath = NULL;
	unsigned short newFlags = (unsigned short)-1;
	if (!node)
		newPath = pathname;
	else
	{
		if (!(node->exportType))
		{
			newPath = node->value;
			newFlags = node->oflags;
		}
		else
		{
			node->exportPath = (char *)genTemporaryFilename ();
			newPath = node->exportPath;
			exportConfiguration (newPath, node);
		}
	}
	if (newFlags == O_RDONLY)
	{
		flags = (flags & (~(0 | O_WRONLY | O_APPEND)));
	}

	orig_open_f_type orig_open64;
	orig_open64 = (orig_open_f_type)dlsym (RTLD_NEXT, "open64");

	int fd;
	if (flags & O_CREAT)
	{
		int mode;
		va_list argptr;
		va_start (argptr, flags);
		mode = va_arg (argptr, int);
		va_end (argptr);
		fd = orig_open64 (newPath, flags, mode);
	}
	else
	{
		fd = orig_open64 (newPath, flags);
	}
	return fd;
}
