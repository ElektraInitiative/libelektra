typedef struct
{
	char * relPath;
	char * dirname;
	char * fullPath;
	char * tmpFile;
} ElektraResolved;

typedef enum
{
	ELEKTRA_RESOLVER_TEMPFILE_NONE,
	ELEKTRA_RESOLVER_TEMPFILE_SAMEDIR,
	ELEKTRA_RESOLVER_TEMPFILE_TMPDIR,
} ElektraResolveTempfile;
