/**
 * @file
 *
 * @brief Source for gitresolver plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include "gitresolver.h"

#include <fcntl.h>
#include <git2.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#define TV_MAX_DIGITS 26
#define DEFAULT_CHECKOUT_LOCATION "/tmp/"
#define REFSTRING "refs/heads/"

typedef enum {
	OBJECT,
	HEAD,
} Tracking;

typedef struct
{
	char * tmpFile;    // temporary filename for checkout
	char * repo;       // path to repo (currently only local)
	char * branch;     // branchname
	char * file;       // filename
	char * refName;    // git reference name e.g. refs/heads/master
	char * headID;     // id of the most recent commit
	char * objID;      // most recent id of the file
	Tracking tracking; // track commit ids or object ids
	int setPhase;      // Set phase counter, 0 setresolver, 1 commit
	time_t mtime;      // creation timestamp of tmp file
} GitData;


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
	snprintf (data->tmpFile, len, "%s%s_%s_%lu:%lu", DEFAULT_CHECKOUT_LOCATION, data->branch, fileName, tv.tv_sec, tv.tv_usec);
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
		data->repo = (char *)keyString (key);
		if (!key)
		{
			ELEKTRA_SET_ERROR (34, parentKey, "no repository specified");
			return -1;
		}

		// default to master branch when no branchname is supplied
		const char * defaultBranch = "master";
		key = ksLookupByName (config, "/branch", KDB_O_NONE);
		if (!key)
			data->branch = (char *)defaultBranch;
		else
			data->branch = (char *)keyString (key);

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
		size_t refLen = strlen (REFSTRING) + strlen (data->branch) + 1;
		data->refName = elektraCalloc (refLen);
		snprintf (data->refName, refLen, "%s%s", REFSTRING, data->branch);

		elektraPluginSetData (handle, data);
	}
	return 0;
}

static git_repository * connectToLocalRepo (GitData * data)
{
	git_libgit2_init ();
	git_repository * repo;
	int rc;
	rc = git_repository_open_ext (&(repo), data->repo, 0, NULL);
	if (rc)
	{
		return NULL;
	}
	const char * repoPath = git_repository_workdir (repo);
	data->file = data->repo + strlen (repoPath);
	return repo;
}

static const git_oid * getHeadRef (GitData * data, git_repository * repo)
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

	const git_oid * headObj = git_reference_target (headRef);
	git_reference_free (headRef);
	return headObj;
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
	char spec[strlen (data->refName) + strlen (data->file) + 2];
	snprintf (spec, sizeof (spec), "%s:%s", data->refName, data->file);
	int rc = git_revparse_single (&blob, repo, spec);
	if (rc)
	{
		// file doesn't exist in repo
		return NULL;
	}
	return blob;
}

int elektraGitresolverGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/gitresolver"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system/elektra/modules/gitresolver", KEY_VALUE, "gitresolver plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/gitresolver/exports", KEY_END),
			keyNew ("system/elektra/modules/gitresolver/exports/open", KEY_FUNC, elektraGitresolverOpen, KEY_END),
			keyNew ("system/elektra/modules/gitresolver/exports/close", KEY_FUNC, elektraGitresolverClose, KEY_END),
			keyNew ("system/elektra/modules/gitresolver/exports/get", KEY_FUNC, elektraGitresolverGet, KEY_END),
			keyNew ("system/elektra/modules/gitresolver/exports/set", KEY_FUNC, elektraGitresolverSet, KEY_END),
			keyNew ("system/elektra/modules/gitresolver/exports/error", KEY_FUNC, elektraGitresolverError, KEY_END),
			keyNew ("system/elektra/modules/gitresolver/exports/checkfile", KEY_FUNC, elektraGitresolverCheckFile, KEY_END),

#include ELEKTRA_README (gitresolver)
			keyNew ("system/elektra/modules/gitresolver/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
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
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_GITRESOLVER_RESOLVING_ISSUE, parentKey, "Failed to open Repository %s\n", data->repo);
		git_libgit2_shutdown ();
		return -1;
	}

	genCheckoutFileName (data);

	keySetString (parentKey, data->tmpFile);

	// TODO: check for empty repo and initialize repo
	const git_oid * headObj = getHeadRef (data, repo);
	if (!headObj)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_GITRESOLVER_RESOLVING_ISSUE, parentKey, "Failed to get reference %s\n", data->refName);
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
	git_object * blob = getBlob (data, repo);
	if (!blob)
	{
		ELEKTRA_ADD_WARNINGF (83, parentKey, "File %s not found in repository %s\n", data->file, data->repo);
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
	outFile = fopen (data->tmpFile, "w+");
	if (!outFile)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_COULD_NOT_OPEN, parentKey, "Failed to check out file %s to %s\n", data->file,
				    data->tmpFile);
		git_object_free (blob);
		git_repository_free (repo);
		git_libgit2_shutdown ();
		return -1;
	}
	fwrite (git_blob_rawcontent ((git_blob *)blob), (size_t)git_blob_rawsize ((git_blob *)blob), 1, outFile);
	struct stat buf;
	int fd;
	fd = fileno (outFile);
	if (fstat (fd, &buf) == -1)
	{
		// this shouldn't happen anyway
	}
	data->mtime = buf.st_mtime;

	fclose (outFile);
	git_object_free (blob);
	git_repository_free (repo);
	git_libgit2_shutdown ();
	return 1; // success
}

static void addFileToIndex (git_repository * repo, GitData * data, git_index * index)
{
	git_blob * blob;
	git_oid blobID;

	git_index_entry ie;
	ie.path = data->file;
	ie.mode = GIT_FILEMODE_BLOB;
	git_blob_create_fromdisk (&blobID, repo, data->tmpFile);
	git_blob_lookup (&blob, repo, &blobID);
	git_index_add_frombuffer (index, &ie, git_blob_rawcontent (blob), git_blob_rawsize (blob));
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
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_GITRESOLVER_RESOLVING_ISSUE, parentKey, "Failed to open Repository %s\n", data->repo);
		git_libgit2_shutdown ();
		return -1;
	}
	const git_oid * headObj = getHeadRef (data, repo);
	if (!headObj)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_GITRESOLVER_RESOLVING_ISSUE, parentKey, "Failed to get reference %s\n", data->refName);
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
			ELEKTRA_SET_ERROR (ELEKTRA_ERROR_GITRESOLVER_CONFLICT, parentKey,
					   "The repository has been updated and is ahead of you");
			elektraFree (newCommit);
			git_repository_free (repo);
			git_libgit2_shutdown ();
			return -1;
		}
		elektraFree (newCommit);
	}
	if (data->tracking == OBJECT)
	{
		git_object * blob = getBlob (data, repo);
		if (blob)
		{
			char * newObj = hasNewObjectCommit (data, blob);
			if (newObj)
			{
				ELEKTRA_SET_ERROR (ELEKTRA_ERROR_GITRESOLVER_CONFLICT, parentKey,
						   "The repository has been updated and is ahead of you");
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
		// commit phase
		int fd = open (data->tmpFile, O_RDONLY);
		struct stat buf;
		fstat (fd, &buf);
		close (fd);
		if (data->mtime == buf.st_mtime)
		{
			// file hasn't changed, nothing to do here
			git_repository_free (repo);
			git_libgit2_shutdown ();
			return 0;
		}
		// get repo index
		git_index * index;
		git_repository_index (&index, repo);

		// add file
		addFileToIndex (repo, data, index);

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
		git_commit_create (&commitID, repo, "HEAD", sig, sig, NULL, "kdb git autocommit", tree, 1, (const git_commit **)&parent);


		git_signature_free (sig);
		git_commit_free (parent);
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

Plugin * ELEKTRA_PLUGIN_EXPORT (gitresolver)
{
	// clang-format off
    return elektraPluginExport ("gitresolver",
            ELEKTRA_PLUGIN_OPEN,	&elektraGitresolverOpen,
            ELEKTRA_PLUGIN_CLOSE,	&elektraGitresolverClose,
            ELEKTRA_PLUGIN_GET,	&elektraGitresolverGet,
            ELEKTRA_PLUGIN_SET,	&elektraGitresolverSet,
            ELEKTRA_PLUGIN_ERROR,	&elektraGitresolverError,
            ELEKTRA_PLUGIN_END);
}

