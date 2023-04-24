/**
 * @file
 *
 * @brief Source for gitresolver plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */


#include <elektra/kdb/errors.h>
#include <fcntl.h>
#include <git2.h>
#include <internal/utility/old_helper.h>
#include <libgen.h>
#include <openssl/md5.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#include "../resolver/shared.h"
#include <elektra/plugin/invoke.h>

#include "./gitresolver.h"

#define TV_MAX_DIGITS 26
#define DEFAULT_CHECKOUT_LOCATION "/tmp/"
#define REFSTRING "refs/heads/"

typedef enum
{
	OBJECT,
	HEAD,
} Tracking;

typedef struct
{
	char * tmpFile;				   // temporary filename for checkout
	char * repo;				   // path to repo (currently only local)
	char * workdir;				   // repository workdir
	char * subdirs;				   // subdirectores between file and workdir
	char * branch;				   // branchname
	char * file;				   // filename
	char * refName;				   // git reference name e.g. refs/heads/master
	char * headID;				   // id of the most recent commit
	char * objID;				   // most recent id of the file
	Tracking tracking;			   // track commit ids or object ids
	int setPhase;				   // Set phase counter, 0 setresolver, 1 commit
	mode_t dirmode;				   //
	unsigned char lastHash[MD5_DIGEST_LENGTH]; // hash of the checkdout file
	short checkout;				   // 1 = checkout file to repo, 0 = checkout to temporary file
	short pull;				   // 0 = don't pull repo, 1 = pull repo
} GitData;


static int elektraResolveFilename (Key * parentKey, ElektraResolveTempfile tmpFile)
{
	int rc = 0;
	ElektraInvokeHandle * handle = elektraInvokeOpen ("resolver", 0, 0);
	if (!handle)
	{
		rc = -1;
		goto RESOLVE_FAILED;
	}
	ElektraResolved * resolved = NULL;
	typedef ElektraResolved * (*resolveFileFunc) (elektraNamespace, const char *, ElektraResolveTempfile, Key *);
	resolveFileFunc resolveFunc = *(resolveFileFunc *) elektraInvokeGetFunction (handle, "filename");

	if (!resolveFunc)
	{
		rc = -1;
		goto RESOLVE_FAILED;
	}

	typedef void (*freeHandleFunc) (ElektraResolved *);
	freeHandleFunc freeHandle = *(freeHandleFunc *) elektraInvokeGetFunction (handle, "freeHandle");

	if (!freeHandle)
	{
		rc = -1;
		goto RESOLVE_FAILED;
	}

	resolved = resolveFunc (keyGetNamespace (parentKey), keyString (parentKey), tmpFile, parentKey);

	if (!resolved)
	{
		rc = -1;
		goto RESOLVE_FAILED;
	}
	else
	{
		keySetString (parentKey, resolved->fullPath);
		freeHandle (resolved);
	}

RESOLVE_FAILED:
	elektraInvokeClose (handle, 0);
	return rc;
}
int elektraGitresolverCheckFile (const char * filename)
{
	if (filename[0] == '/') return 0;

	return 1;
}

static void genCheckoutFileName (GitData * data)
{
	// generate temp filename: /tmp/branch_filename_tv_sec:tv_usec
	struct timeval tv;
	gettimeofday (&tv, 0);
	const char * fileName = strrchr (data->file, '/');
	if (!fileName)
		fileName = data->file;
	else
		fileName += 1;
	size_t len = strlen (DEFAULT_CHECKOUT_LOCATION) + strlen (data->branch) + strlen (fileName) + TV_MAX_DIGITS + 1;
	data->tmpFile = elektraCalloc (len);
	snprintf (data->tmpFile, len, "%s%s_%s_%lu:" ELEKTRA_TIME_USEC_F, DEFAULT_CHECKOUT_LOCATION, data->branch, fileName, tv.tv_sec,
		  tv.tv_usec);
}

static unsigned char * hashBuffer (const void * buffer, size_t size)
{
	MD5_CTX c;
	unsigned char * out = elektraMalloc (MD5_DIGEST_LENGTH);
	MD5_Init (&c);
	MD5_Update (&c, buffer, size);
	MD5_Final (out, &c);
	return out;
}

int elektraGitresolverOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	// this function is optional
	return 1; // success
}

int elektraGitresolverClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional
	GitData * data = elektraPluginGetData (handle);
	if (!data) return 0;
	if (data->headID) elektraFree (data->headID);
	if (data->tmpFile)
	{
		unlink (data->tmpFile); // remove temporary checked out file when closing
		elektraFree (data->tmpFile);
	}
	if (data->repo) elektraFree (data->repo);
	if (data->workdir) elektraFree (data->workdir);
	if (data->subdirs) elektraFree (data->subdirs);
	if (data->file) elektraFree (data->file);
	if (data->refName) elektraFree (data->refName);
	elektraFree (data);
	elektraPluginSetData (handle, NULL);
	return 1; // success
}
static int initData (Plugin * handle, Key * parentKey)
{

	GitData * data = elektraPluginGetData (handle);
	if (!data)
	{
		KeySet * config = elektraPluginGetConfig (handle);
		data = elektraCalloc (sizeof (GitData));

		Key * key = ksLookupByName (config, "/path", KDB_O_NONE);
		keySetString (parentKey, keyString (key));
		if (elektraResolveFilename (parentKey, ELEKTRA_RESOLVER_TEMPFILE_NONE) == -1)
		{
			return -1;
		}
		data->repo = elektraStrDup (keyString (parentKey));

		// default to master branch when no branchname is supplied
		const char * defaultBranch = "master";
		key = ksLookupByName (config, "/branch", KDB_O_NONE);
		if (!key)
			data->branch = (char *) defaultBranch;
		else
			data->branch = (char *) keyString (key);

		key = ksLookupByName (config, "/tracking", KDB_O_NONE);
		if (!key)
			data->tracking = HEAD;
		else
		{
			if (!strcmp (keyString (key), "object"))
				data->tracking = OBJECT;
			else
				data->tracking = HEAD;
		}
		key = ksLookupByName (config, "/pull", KDB_O_NONE);
		if (!key)
		{
			data->pull = 0;
		}
		else
		{
			data->pull = 1;
		}
		size_t refLen = strlen (REFSTRING) + strlen (data->branch) + 1;
		data->refName = elektraCalloc (refLen);
		snprintf (data->refName, refLen, "%s%s", REFSTRING, data->branch);
		key = ksLookupByName (config, "/checkout", KDB_O_NONE);
		if (!key)
		{
			data->checkout = 0;
		}
		else
		{
			data->checkout = 1;
		}
		elektraPluginSetData (handle, data);
	}
	return 0;
}

static git_buf * discover_repo (char * path)
{
	if (!strcmp (path, "/")) return NULL;

	git_buf * buf = elektraCalloc (sizeof (git_buf));
	int rc = git_repository_discover (buf, path, 0, NULL);
	if (rc)
	{
		git_buf_free (buf);
		elektraFree (buf);
		buf = discover_repo (dirname (path));
		return buf;
	}
	else
	{
		return buf;
	}
}

static git_repository * connectToLocalRepo (GitData * data)
{
	git_libgit2_init ();
	git_repository * repo;
	int rc;
	git_buf buf = GIT_BUF_INIT_CONST (NULL, 0);
	rc = git_repository_discover (&buf, data->repo, 0, NULL);
	if (rc)
	{
		char * repoCopy = elektraStrDup (data->repo);
		git_buf * discovered = discover_repo (dirname (repoCopy));
		if (discovered)
		{
			if (data->workdir) elektraFree (data->workdir);
			data->workdir = elektraStrDup (discovered->ptr);
			elektraFree (repoCopy);
			git_buf_free (discovered);
			elektraFree (discovered);
		}
		else
		{
			git_buf_free (&buf);
			elektraFree (repoCopy);
			return NULL;
		}
	}
	else
	{
		if (data->workdir) elektraFree (data->workdir);
		data->workdir = elektraStrDup (buf.ptr);
	}
	git_buf_free (&buf);
	rc = git_repository_open_ext (&(repo), data->workdir, 0, NULL);
	if (rc)
	{
		return NULL;
	}
	const char * repoPath = git_repository_workdir (repo);
	if (data->workdir) elektraFree (data->workdir);
	data->workdir = elektraStrDup (repoPath);
	struct stat buffer;
	if (stat (data->workdir, &buffer) == -1)
	{
		data->dirmode = 0100;
	}
	else
	{
		data->dirmode = buffer.st_mode;
	}
	char * repoCopy = elektraStrDup (data->repo);
	char * dir = dirname (repoCopy);
	if (data->subdirs) elektraFree (data->subdirs);
	data->subdirs = elektraStrDup (dir + elektraStrLen (data->workdir) - 2);
	if (!strcmp (data->subdirs, ""))
	{
		elektraFree (data->subdirs);
		data->subdirs = NULL;
	}
	elektraFree (repoCopy);
	repoCopy = elektraStrDup (data->repo);
	if (data->file) elektraFree (data->file);
	data->file = elektraStrDup (basename (data->repo));
	elektraFree (repoCopy);
	return repo;
}

static git_reference * getHeadRef (GitData * data, git_repository * repo)
{
	git_reference * headRef;
	int rc = git_reference_lookup (&headRef, repo, data->refName);
	if (rc)
	{
		git_reference_free (headRef);
		return NULL;
	}

	// compare newest commit id to last saved commit id
	// only update if there's a newer commit

	// const git_oid * headObj = git_reference_target (headRef);
	// git_reference_free (headRef);
	//    return headObj;
	return headRef;
}

static char * hasNewCommit (GitData * data, const git_oid * headObj)
{
	size_t IDSize = GIT_OID_HEXSZ + 1;
	char * commitID = elektraCalloc (IDSize);
	git_oid_tostr (commitID, IDSize, headObj);
	if (!data->headID)
	{
		return commitID;
	}
	else
	{
		if (!strcmp (data->headID, commitID))
		{
			elektraFree (commitID);
			return NULL;
		}
		else
		{
			return commitID;
		}
	}
}
static char * hasNewObjectCommit (GitData * data, git_object * blob)
{
	size_t IDSize = GIT_OID_HEXSZ + 1;
	char * objID = elektraCalloc (IDSize);
	git_oid_tostr (objID, IDSize, git_object_id (blob));
	if (!data->objID)
	{
		return objID;
	}
	else
	{
		if (!strcmp (data->objID, objID))
		{
			elektraFree (objID);
			return NULL;
		}
		else
		{
			return objID;
		}
	}
}


static git_object * getBlob (GitData * data, git_repository * repo)
{
	git_object * blob;
	size_t specSize = strlen (data->refName) + strlen (data->file) + 3;
	if (data->subdirs) specSize += strlen (data->subdirs);
	char spec[specSize];
	if (!data->subdirs)
	{
		snprintf (spec, sizeof (spec), "%s:%s", data->refName, data->file);
	}
	else
	{
		snprintf (spec, sizeof (spec), "%s:%s/%s", data->refName, (data->subdirs) + 1, data->file);
	}
	int rc = git_revparse_single (&blob, repo, spec);
	if (rc)
	{
		// file doesn't exist in repo
		return NULL;
	}
	return blob;
}

// inspired by bash make_path
static void makePath (GitData * data)
{
	if (!data->subdirs) return;
	char * path = elektraStrDup (data->repo);
	char * ptr = strrchr (path, '/');
	*ptr = '\0';
	ptr = path + strlen (data->workdir) - 2;
	struct stat sb;
	while ((ptr = strchr (ptr, '/')) != NULL)
	{
		*ptr = '\0';
		if (stat (path, &sb) != 0)
		{
			if (mkdir (path, data->dirmode))
			{
				elektraFree (path);
				return;
			}
		}
		else if (S_ISDIR (sb.st_mode) == 0)
		{
			elektraFree (path);
			return;
		}
		*ptr++ = '/';
	}
	if (stat (path, &sb) && mkdir (path, data->dirmode)) elektraFree (path);
	return;
	elektraFree (path);
}

typedef struct
{
	char * branchName;
	git_oid * oid;
} fetch_cb_data;

static int fetchhead_ref_cb (const char * name, const char * url ELEKTRA_UNUSED, const git_oid * oid, unsigned int is_merge, void * payload)
{
	if (is_merge)
	{
		fetch_cb_data * data = payload;
		data->branchName = elektraStrDup (name);
		data->oid = elektraCalloc (sizeof (git_oid));
		memcpy (data->oid, oid, sizeof (git_oid));
	}
	return 0;
}

typedef enum
{
	ERROR,
	NONE,
	NORMAL,
	UPTODATE,
	FASTFORWARD,
	UNBORN,
} MergeAnalysis;


static MergeAnalysis mergeAnalysis (git_repository * repo, const git_annotated_commit ** heads)
{
	git_merge_analysis_t analysis;
	git_merge_preference_t preference;
	int rc = git_merge_analysis (&analysis, &preference, repo, (const git_annotated_commit **) heads, 1);
	if (rc < 0)
	{
		return ERROR;
	}
	if (analysis & GIT_MERGE_ANALYSIS_FASTFORWARD)
	{
		return FASTFORWARD;
	}
	else if (analysis & GIT_MERGE_ANALYSIS_UP_TO_DATE)
	{
		return UPTODATE;
	}
	else if (analysis & GIT_MERGE_ANALYSIS_NORMAL)
	{
		return NORMAL;
	}
	else if (analysis & GIT_MERGE_ANALYSIS_NONE)
	{
		return NONE;
	}
	else if (analysis & GIT_MERGE_ANALYSIS_UNBORN)
	{
		return UNBORN;
	}
	else
	{
		return ERROR;
	}
}

static int doMerge (git_repository * repo, const git_annotated_commit ** heads)
{
	git_merge_options mergeOpts = { GIT_MERGE_OPTIONS_VERSION, .flags = GIT_MERGE_FAIL_ON_CONFLICT };
	git_checkout_options checkoutOpts = { GIT_CHECKOUT_OPTIONS_VERSION, .checkout_strategy = GIT_CHECKOUT_FORCE };

	int rc = git_merge (repo, (const git_annotated_commit **) heads, 1, &mergeOpts, &checkoutOpts);
	if (rc < 0)
	{
		return -1;
	}
	return 0;
}

static int setFFTarget (git_repository * repo, git_oid * oid)
{
	git_reference * out;
	git_reference * ref;
	git_repository_head (&ref, repo);

	int rc = git_reference_set_target (&out, ref, oid, "fastforward");
	if (rc)
	{
		return -1;
	}
	return 0;
}

static int ELEKTRA_UNUSED hasMergeConflicts (git_repository * repo)
{
	git_index * cIdx;
	int hasConflicts = 0;
	git_repository_index (&cIdx, repo);
	hasConflicts = git_index_has_conflicts (cIdx);
	git_index_free (cIdx);
	if (hasConflicts)
	{
		return -1;
	}
	return 0;
}

static int pullFromRemote (GitData * data ELEKTRA_UNUSED, git_repository * repo)
{
	git_remote * remote;
	int rc = git_remote_lookup (&remote, repo, "origin");
	if (rc < 0)
	{
		return -1;
	}
	rc = git_remote_fetch (remote, NULL, NULL, NULL);
	if (rc < 0)
	{
		return -1;
	}
	fetch_cb_data * cb_data = elektraCalloc (sizeof (fetch_cb_data));

	git_repository_fetchhead_foreach (repo, fetchhead_ref_cb, cb_data);

	git_annotated_commit * heads[1];
	rc = git_annotated_commit_lookup (&heads[0], repo, cb_data->oid);
	if (rc < 0)
	{
		return -1;
	}

	MergeAnalysis res = mergeAnalysis (repo, (const git_annotated_commit **) heads);
	rc = 0;
	switch (res)
	{
	case ERROR:
	case UNBORN:
		rc = -1;
		goto PULL_CLEANUP;
		break;
	case NONE:
		goto PULL_CLEANUP;
		break;
	case UPTODATE:
		goto PULL_CLEANUP;
		break;
	case FASTFORWARD:
		rc = doMerge (repo, (const git_annotated_commit **) heads);
		if (rc)
		{
			goto PULL_CLEANUP;
		}
		rc = setFFTarget (repo, cb_data->oid);
		if (rc)
		{
			goto PULL_CLEANUP;
		}
		rc = 0;
		break;
	case NORMAL:
		// rc = doMerge(repo, heads);
		// if(rc)
		//{
		//    goto PULL_CLEANUP;
		//}
		// rc = hasMergeConflicts(repo);
		// if(rc)
		//{
		//    goto PULL_CLEANUP;
		//}
		rc = -1;
		goto PULL_CLEANUP;
		break;
	default:
		rc = -1;
		goto PULL_CLEANUP;
		break;
	}

PULL_CLEANUP:
	git_annotated_commit_free (heads[0]);
	git_repository_state_cleanup (repo);

	return rc;
}

int elektraGitresolverGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/gitresolver"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system:/elektra/modules/gitresolver", KEY_VALUE, "gitresolver plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/gitresolver/exports", KEY_END),
			keyNew ("system:/elektra/modules/gitresolver/exports/open", KEY_FUNC, elektraGitresolverOpen, KEY_END),
			keyNew ("system:/elektra/modules/gitresolver/exports/close", KEY_FUNC, elektraGitresolverClose, KEY_END),
			keyNew ("system:/elektra/modules/gitresolver/exports/get", KEY_FUNC, elektraGitresolverGet, KEY_END),
			keyNew ("system:/elektra/modules/gitresolver/exports/set", KEY_FUNC, elektraGitresolverSet, KEY_END),
			keyNew ("system:/elektra/modules/gitresolver/exports/commit", KEY_FUNC, elektraGitresolverCommit, KEY_END),
			keyNew ("system:/elektra/modules/gitresolver/exports/error", KEY_FUNC, elektraGitresolverError, KEY_END),
			keyNew ("system:/elektra/modules/gitresolver/exports/checkfile", KEY_FUNC, elektraGitresolverCheckFile, KEY_END),

#include ELEKTRA_README
			keyNew ("system:/elektra/modules/gitresolver/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys

	if (initData (handle, parentKey)) return -1;
	GitData * data = elektraPluginGetData (handle);
	git_repository * repo = connectToLocalRepo (data);
	if (!repo)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Failed to open Repository %s\n", data->repo);
		git_libgit2_shutdown ();
		return -1;
	}

	genCheckoutFileName (data);

	// TODO: check for empty repo and initialize repo
	git_reference * headRef = getHeadRef (data, repo);
	if (!headRef)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Failed to get reference %s\n", data->refName);
		git_repository_free (repo);
		git_libgit2_shutdown ();
		return -1;
	}
	if (data->pull)
	{
		int rc = pullFromRemote (data, repo);
		if (rc)
		{
			ELEKTRA_SET_CONFLICTING_STATE_ERROR (parentKey, "Fast-forward pull failed, please pull manually\n");
			git_repository_free (repo);
			git_libgit2_shutdown ();
			return -1;
		}
	}
	const git_oid * headObj = git_reference_target (headRef);
	if (!headObj)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Failed to get reference %s\n", data->refName);
		git_reference_free (headRef);
		git_repository_free (repo);
		git_libgit2_shutdown ();
		return -1;
	}
	if (data->tracking == HEAD)
	{
		char * newCommit = hasNewCommit (data, headObj);
		if (data->headID && !newCommit)
		{
			// still newest commit, no need to update
			git_reference_free (headRef);
			git_repository_free (repo);
			git_libgit2_shutdown ();
			return 0;
		}
		else if (data->headID && newCommit)
		{
			elektraFree (data->headID);
			data->headID = newCommit;
		}
		else
		{
			data->headID = newCommit;
		}
		elektraPluginSetData (handle, data);
		data = elektraPluginGetData (handle);
	}
	git_reference_free (headRef);
	git_object * blob = getBlob (data, repo);
	if (!blob)
	{
		ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "File %s not found in repository %s\n", data->file, data->repo);
		git_repository_free (repo);
		git_libgit2_shutdown ();
		return 0;
	}
	if (data->tracking == OBJECT)
	{
		char * newObj = hasNewObjectCommit (data, blob);
		if (!newObj)
		{
			git_object_free (blob);
			git_repository_free (repo);
			git_libgit2_shutdown ();
			return 0;
		}
		else
		{
			if (data->objID)
			{
				elektraFree (data->objID);
				data->objID = newObj;
			}
			else
			{
				data->objID = newObj;
			}
		}
		elektraPluginSetData (handle, data);
		data = elektraPluginGetData (handle);
	}
	FILE * outFile;
	if (!data->checkout)
	{
		keySetString (parentKey, data->tmpFile);
	}
	else
	{
		keySetString (parentKey, data->repo);
		if (data->subdirs) makePath (data);
	}
	outFile = fopen (keyString (parentKey), "w+");
	if (!outFile)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Failed to check out file %s to %s\n", data->file, keyString (parentKey));
		git_object_free (blob);
		git_repository_free (repo);
		git_libgit2_shutdown ();
		return -1;
	}
	fwrite (git_blob_rawcontent ((git_blob *) blob), (size_t) git_blob_rawsize ((git_blob *) blob), 1, outFile);
	unsigned char * hash = hashBuffer (git_blob_rawcontent ((git_blob *) blob), git_blob_rawsize ((git_blob *) blob));
	if (!*(data->lastHash)) memcpy (data->lastHash, hash, MD5_DIGEST_LENGTH);
	elektraFree (hash);
	fclose (outFile);
	git_object_free (blob);
	git_repository_free (repo);
	git_libgit2_shutdown ();
	return 1; // success
}

static git_blob * addFileToIndex (git_repository * repo, GitData * data, git_index * index)
{
	git_blob * blob = NULL;
	git_oid blobID;
	memset (&blobID, 0, sizeof (git_oid));
	git_index_entry ie;
	memset (&ie, 0, sizeof (git_index_entry));
	ie.path = data->repo + elektraStrLen (data->workdir) - 1;
	ie.mode = GIT_FILEMODE_BLOB;
	git_blob_create_fromdisk (&blobID, repo, data->tmpFile);
	git_blob_lookup (&blob, repo, &blobID);
	git_index_add_frombuffer (index, &ie, git_blob_rawcontent (blob), git_blob_rawsize (blob));
	return blob;
}

static int moveFile (const char * source, const char * dest)
{
	FILE * inFile = NULL;
	FILE * outFile = NULL;
	struct stat buf;
	if (stat (source, &buf) == -1) return -1;
	size_t fileSize = buf.st_size;
	char * buffer = elektraMalloc (fileSize);
	inFile = fopen (source, "rb");
	size_t bytesRead = 0;
	while (bytesRead < fileSize)
	{
		size_t bytes = fread (buffer + bytesRead, 1, (size_t) fileSize, inFile);
		if (bytes == 0) break;
		bytesRead += bytes;
	}
	if (bytesRead < fileSize)
	{
		elektraFree (buffer);
		fclose (inFile);
		return -1;
	}
	fclose (inFile);
	outFile = fopen (dest, "wb+");

	size_t bytesWritten = 0;
	while (bytesWritten < fileSize)
	{
		size_t bytes = fwrite (buffer, 1, fileSize, outFile);
		if (bytes == 0) break;
		bytesWritten += bytes;
	}
	fclose (outFile);
	elektraFree (buffer);

	if (bytesWritten < fileSize)
	{
		return -1;
	}
	unlink (source);
	return 0;
}

int elektraGitresolverSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// get all keys
	// this function is optional
	GitData * data = elektraPluginGetData (handle);
	if (!data) return -1;
	keySetString (parentKey, data->tmpFile);
	git_repository * repo = connectToLocalRepo (data);
	if (!repo)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Failed to open Repository %s\n", data->repo);
		git_libgit2_shutdown ();
		return -1;
	}
	git_reference * headRef = getHeadRef (data, repo);
	if (!headRef)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Failed to get reference %s\n", data->refName);
		git_repository_free (repo);
		git_libgit2_shutdown ();
		return -1;
	}
	const git_oid * headObj = git_reference_target (headRef);
	if (!headObj)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Failed to get reference %s\n", data->refName);
		git_reference_free (headRef);
		git_repository_free (repo);
		git_libgit2_shutdown ();
		return -1;
	}

	if (data->tracking == HEAD)
	{
		char * newCommit = hasNewCommit (data, headObj);
		if (newCommit)
		{
			// newer commit in repo - abort
			ELEKTRA_SET_CONFLICTING_STATE_ERROR (parentKey, "The repository has been updated and is ahead of you");
			elektraFree (newCommit);
			git_reference_free (headRef);
			git_repository_free (repo);
			git_libgit2_shutdown ();
			return -1;
		}
		elektraFree (newCommit);
	}
	git_reference_free (headRef);
	if (data->tracking == OBJECT)
	{
		git_object * blob = getBlob (data, repo);
		if (blob)
		{
			char * newObj = hasNewObjectCommit (data, blob);
			if (newObj)
			{
				ELEKTRA_SET_CONFLICTING_STATE_ERROR (parentKey, "The repository has been updated and is ahead of you");
				elektraFree (newObj);
				git_object_free (blob);
				git_repository_free (repo);
				git_libgit2_shutdown ();
				return -1;
			}
			git_object_free (blob);
		}
	}
	if (!data->setPhase)
	{
		++(data->setPhase);
	}
	else if (data->setPhase == 1)
	{
		// get repo index
		git_index * index;
		git_repository_index (&index, repo);

		// add file
		git_blob * buffer = addFileToIndex (repo, data, index);
		unsigned char * hash = hashBuffer ((unsigned char *) git_blob_rawcontent (buffer), git_blob_rawsize (buffer));
		if (!strncmp ((char *) data->lastHash, (char *) hash, MD5_DIGEST_LENGTH))
		{
			elektraFree (hash);
			git_index_free (index);
			git_blob_free (buffer);
			git_repository_free (repo);
			git_libgit2_shutdown ();
			return 0;
		}
		elektraFree (hash);

		git_index_write (index);

		// get tree id
		git_oid treeID;
		git_index_write_tree (&treeID, index);

		// get parent commit
		git_oid parentID;
		git_commit * parent;
		git_reference_name_to_id (&parentID, repo, "HEAD");
		git_commit_lookup (&parent, repo, &parentID);

		// extract default git user
		git_signature * sig;
		int rc = git_signature_default (&sig, repo);
		if (rc == GIT_ENOTFOUND)
		{
			git_signature_now (&sig, "Elektra", "@libelektra.org");
		}

		// get git tree
		git_tree * tree;
		git_tree_lookup (&tree, repo, &treeID);

		// create default commit
		git_oid commitID;
		git_commit_create (&commitID, repo, "HEAD", sig, sig, NULL, "kdb git autocommit", tree, 1, (const git_commit **) &parent);


		git_signature_free (sig);
		git_tree_free (tree);
		git_index_free (index);
		git_blob_free (buffer);
		git_commit_free (parent);
		if (data->checkout)
		{
			moveFile (data->tmpFile, data->repo);
		}
	}
	elektraPluginSetData (handle, data);
	git_repository_free (repo);
	git_libgit2_shutdown ();
	return 1; // success
}

int elektraGitresolverError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// set all keys
	// this function is optional

	return 1; // success
}

int elektraGitresolverCommit (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	return elektraGitresolverSet (handle, returned, parentKey);
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
    return elektraPluginExport ("gitresolver",
            ELEKTRA_PLUGIN_OPEN,	&elektraGitresolverOpen,
            ELEKTRA_PLUGIN_CLOSE,	&elektraGitresolverClose,
            ELEKTRA_PLUGIN_GET,	&elektraGitresolverGet,
            ELEKTRA_PLUGIN_SET,	&elektraGitresolverSet,
            ELEKTRA_PLUGIN_ERROR,	&elektraGitresolverError,
            ELEKTRA_PLUGIN_COMMIT,	&elektraGitresolverCommit,
            ELEKTRA_PLUGIN_END);
}

