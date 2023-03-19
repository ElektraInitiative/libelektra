/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <tests_internal.h>

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/ease/meta.h>
#include <elektra/ease/old_ease.h>
#include <elektra/kdb/errors.h>
#include <elektra/plugin/plugin.h>
#include <internal/kdb/config.h>
#include <internal/kdbprivate.h>
#include <internal/pluginload/module.h>
#include <internal/utility/logger.h>
#include <langinfo.h>

#include "resolver.h"

// FIXME [new_backend]: tests partially disabled, see TODOs
void test_resolve (void)
{
	int pathLen = tempHomeLen + 1 + strlen (KDB_DB_USER) + 12 + 1;
	char * path = elektraMalloc (pathLen);
	exit_if_fail (path != 0, "elektraMalloc failed");
	snprintf (path, pathLen, "%s/%s/elektra.ecf", tempHome, KDB_DB_USER);

	printf ("Resolve Filename\n");

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	Key * parentKey = keyNew ("system:/", KEY_VALUE, "elektra.ecf", KEY_END);
	Plugin * plugin = elektraPluginOpen ("resolver", modules, ksNew (0, KS_END), 0);
	exit_if_fail (plugin, "could not load resolver plugin");

	succeed_if (plugin->kdbOpen != 0, "no open pointer");
	succeed_if (plugin->kdbClose != 0, "no open pointer");
	succeed_if (plugin->kdbGet != 0, "no open pointer");
	succeed_if (plugin->kdbSet != 0, "no open pointer");
	succeed_if (plugin->kdbError != 0, "no open pointer");
	succeed_if (plugin->kdbCommit != 0, "no open pointer");

	succeed_if (!strncmp (plugin->name, "resolver", strlen ("resolver")), "got wrong name");

	// we need to invoke kdbGet at least once, otherwise the handle below will not yet be initialized
	plugin->kdbGet (plugin, 0, parentKey);

	resolverHandles * h = elektraPluginGetData (plugin);
	exit_if_fail (h != 0, "no plugin handle");

	// TODO: fix tests
	// succeed_if_same_string (h->system.path, "elektra.ecf");

	if (KDB_DB_SYSTEM[0] == '~')
	{
		// only check filename and issue warning
		const char * lastSlash = strrchr (h->system.filename, '/');
		warn_if_fail (0, "using home based KDB_DB_SYSTEM falls back to less strict testing");
		succeed_if (lastSlash != NULL && strcmp (lastSlash, "/elektra.ecf") == 0, "wrong filename with home based KDB_DB_SYSTEM");
	}
	else
	{
		succeed_if_same_string (h->system.filename, KDB_DB_SYSTEM "/elektra.ecf");
	}

	// TODO: fix tests
	// succeed_if_same_string (h->user.path, "elektra.ecf");

	// TODO: fix tests
	// succeed_if_same_string (h->user.filename, path);
	plugin->kdbClose (plugin, parentKey);

	// reinit with system path only
	plugin->kdbOpen (plugin, parentKey);

	// we need to invoke kdbGet at least once, otherwise the handle below will not yet be initialized
	plugin->kdbGet (plugin, 0, parentKey);

	h = elektraPluginGetData (plugin);
	exit_if_fail (h != 0, "no plugin handle");
	succeed_if_same_string (h->system.path, KDB_DB_SYSTEM "/elektra.ecf");
	if (KDB_DB_SYSTEM[0] == '~')
	{
		// only check filename and issue warning
		const char * lastSlash = strrchr (h->system.filename, '/');
		warn_if_fail (0, "using home based KDB_DB_SYSTEM falls back to less strict testing");
		succeed_if (lastSlash != NULL && strcmp (lastSlash, "/elektra.ecf") == 0, "wrong filename with home based KDB_DB_SYSTEM");
	}
	else
	{
		succeed_if_same_string (h->system.filename, KDB_DB_SYSTEM "/elektra.ecf");
	}
	// TODO: https://github.com/ElektraInitiative/libelektra/pull/4192#issuecomment-990075883
	// succeed_if (h->user.filename == NULL, "user was initialized, but is not needed");

	plugin->kdbClose (plugin, parentKey);

	keyDel (parentKey);
	elektraPluginClose (plugin, 0);
	elektraModulesClose (modules, 0);
	ksDel (modules);
	elektraFree (path);
}

void test_name (void)
{
	printf ("Resolve Name\n");

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	Key * parentKey = keyNew ("system:/", KEY_VALUE, "elektra.ecf", KEY_END);
	Plugin * plugin = elektraPluginOpen ("resolver", modules, ksNew (0, KS_END), 0);
	exit_if_fail (plugin, "could not load resolver plugin");

	succeed_if (plugin->kdbOpen != 0, "no open pointer");
	succeed_if (plugin->kdbClose != 0, "no open pointer");
	succeed_if (plugin->kdbGet != 0, "no open pointer");
	succeed_if (plugin->kdbSet != 0, "no open pointer");
	succeed_if (plugin->kdbError != 0, "no open pointer");
	succeed_if (plugin->kdbCommit != 0, "no open pointer");

	succeed_if (!strncmp (plugin->name, "resolver", strlen ("resolver")), "got wrong name");


	// we need to invoke kdbGet at least once, otherwise the handle below will not yet be initialized
	plugin->kdbGet (plugin, 0, parentKey);

	resolverHandles * h = elektraPluginGetData (plugin);
	succeed_if (h != 0, "no plugin handle");

	if (KDB_DB_SYSTEM[0] == '~')
	{
		// only check filename and issue warning
		const char * lastSlash = strrchr (keyString (parentKey), '/');
		warn_if_fail (0, "using home based KDB_DB_SYSTEM falls back to less strict testing");
		succeed_if (lastSlash != NULL && strcmp (lastSlash, "/elektra.ecf") == 0, "wrong filename with home based KDB_DB_SYSTEM");
	}
	else
	{
		succeed_if_same_string (keyString (parentKey), KDB_DB_SYSTEM "/elektra.ecf");
	}

	keyDel (parentKey);
	elektraPluginClose (plugin, 0);
	elektraModulesClose (modules, 0);
	ksDel (modules);
}

void test_lockname (void)
{
	printf ("Resolve Dirname\n");

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	Key * parentKey = keyNew ("system:/", KEY_VALUE, "elektra.ecf", KEY_END);
	Plugin * plugin = elektraPluginOpen ("resolver", modules, ksNew (0, KS_END), 0);
	exit_if_fail (plugin, "could not load resolver plugin");

	succeed_if (plugin->kdbOpen != 0, "no open pointer");
	succeed_if (plugin->kdbClose != 0, "no open pointer");
	succeed_if (plugin->kdbGet != 0, "no open pointer");
	succeed_if (plugin->kdbSet != 0, "no open pointer");
	succeed_if (plugin->kdbError != 0, "no open pointer");
	succeed_if (plugin->kdbCommit != 0, "no open pointer");

	succeed_if (!strncmp (plugin->name, "resolver", strlen ("resolver")), "got wrong name");

	// we need to invoke kdbGet at least once, otherwise the handle below will not yet be initialized
	plugin->kdbGet (plugin, 0, parentKey);

	resolverHandles * h = elektraPluginGetData (plugin);
	succeed_if (h != 0, "no plugin handle");

	if (h && KDB_DB_SYSTEM[0] == '~')
	{
		// issue warning and then only check if dirname ends with the user-independent part of KDB_DB_SYSTEM
		const char * firstSlash = strchr (KDB_DB_SYSTEM, '/');
		warn_if_fail (0, "using home based KDB_DB_SYSTEM falls back to less strict testing");
		succeed_if (firstSlash == NULL ||
				    strcmp (h->system.dirname + strlen (h->system.dirname) - strlen (firstSlash), firstSlash) == 0,
			    "resulting filename not correct with home based KDB_DB_SYSTEM");
	}
	else
	{
		succeed_if (h && !strcmp (h->system.dirname, KDB_DB_SYSTEM), "resulting filename not correct");
	}

	keyDel (parentKey);
	elektraPluginClose (plugin, 0);
	elektraModulesClose (modules, 0);
	ksDel (modules);
}

void test_tempname (void)
{
	printf ("Resolve Tempname\n");

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	Key * parentKey = keyNew ("system:/", KEY_VALUE, "elektra.ecf", KEY_END);
	Plugin * plugin = elektraPluginOpen ("resolver", modules, ksNew (0, KS_END), 0);
	exit_if_fail (plugin, "could not load resolver plugin");

	succeed_if (plugin->kdbOpen != 0, "no open pointer");
	succeed_if (plugin->kdbClose != 0, "no open pointer");
	succeed_if (plugin->kdbGet != 0, "no open pointer");
	succeed_if (plugin->kdbSet != 0, "no open pointer");
	succeed_if (plugin->kdbError != 0, "no open pointer");
	succeed_if (plugin->kdbCommit != 0, "no open pointer");

	succeed_if (!strncmp (plugin->name, "resolver", strlen ("resolver")), "got wrong name");

	// we need to invoke kdbGet at least once, otherwise the handle below will not yet be initialized
	plugin->kdbGet (plugin, 0, parentKey);

	resolverHandles * h = elektraPluginGetData (plugin);
	succeed_if (h != 0, "no plugin handle");

	if (h && KDB_DB_SYSTEM[0] == '~')
	{
		// only check filename and issue warning
		const char * lastSlash = strrchr (h->system.tempfile, '/');
		warn_if_fail (0, "using home based KDB_DB_SYSTEM falls back to less strict testing");
		succeed_if (lastSlash != NULL && !strncmp (lastSlash, "/elektra.ecf", sizeof ("/elektra.ecf") - 1),
			    "resulting filename not correct with home based KDB_DB_SYSTEM");
	}
	else
	{
		succeed_if (h && !strncmp (h->system.tempfile, KDB_DB_SYSTEM "/elektra.ecf", sizeof (KDB_DB_SYSTEM "/elektra.ecf") - 1),
			    "resulting filename not correct");
	}


	keyDel (parentKey);
	elektraPluginClose (plugin, 0);
	elektraModulesClose (modules, 0);
	ksDel (modules);
}

void test_checkfile (void)
{
	printf ("Check file\n");

	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	Plugin * plugin = elektraPluginOpen ("resolver", modules, ksNew (0, KS_END), 0);
	exit_if_fail (plugin, "did not find a resolver");

	Key * root = keyNew ("system:/elektra/modules", KEY_END);
	keyAddBaseName (root, plugin->name);

	KeySet * contract = ksNew (5, KS_END);

	plugin->kdbGet (plugin, contract, root);
	keyAddName (root, "/exports/checkfile");
	Key * found = ksLookup (contract, root, 0);
	exit_if_fail (found, "did not find checkfile symbol");

	typedef int (*func_t) (const char *);
	union
	{
		func_t f;
		void * v;
	} conversation;

	succeed_if (keyGetBinary (found, &conversation.v, sizeof (conversation)) == sizeof (conversation), "could not get binary");
	func_t checkFile = conversation.f;


	succeed_if (checkFile ("valid") == 1, "valid file not recognised");
	succeed_if (checkFile ("/valid") == 0, "valid absolute file not recognised");
	succeed_if (checkFile ("/absolute/valid") == 0, "valid absolute file not recognised");
	succeed_if (checkFile ("../valid") == -1, "invalid file not recognised");
	succeed_if (checkFile ("valid/..") == -1, "invalid file not recognised");
	succeed_if (checkFile ("/../valid") == -1, "invalid absolute file not recognised");
	succeed_if (checkFile ("/valid/..") == -1, "invalid absolute file not recognised");
	succeed_if (checkFile ("very..strict") == -1, "resolver is currently very strict");
	succeed_if (checkFile ("very/..strict") == -1, "resolver is currently very strict");
	succeed_if (checkFile ("very../strict") == -1, "resolver is currently very strict");
	succeed_if (checkFile ("very/../strict") == -1, "resolver is currently very strict");
	succeed_if (checkFile ("/") == -1, "invalid absolute file not recognised");
	succeed_if (checkFile (".") == -1, "invalid file not recognised");
	succeed_if (checkFile ("..") == -1, "invalid file not recognised");

	ksDel (contract);
	keyDel (root);

	elektraPluginClose (plugin, 0);
	elektraModulesClose (modules, 0);
	ksDel (modules);
}

static void check_xdg (void)
{
	KeySet * modules = ksNew (0, KS_END);
	elektraModulesInit (modules, 0);

	Plugin * plugin = elektraPluginOpen ("resolver", modules, ksNew (0, KS_END), 0);
	exit_if_fail (plugin, "did not find a resolver");

	int abort = 0;
	if (strchr (plugin->name, 'x') != NULL)
	{
		printf ("Will abort successfully because default resolver is an XDG resolver (%s)\n", plugin->name);
		abort = 1;
	}

	elektraPluginClose (plugin, 0);
	elektraModulesClose (modules, 0);
	ksDel (modules);

	if (abort)
	{
		exit (0);
	}
}


int main (int argc, char ** argv)
{
	printf ("  RESOLVER  TESTS\n");
	printf ("====================\n\n");

	init (argc, argv);

	test_checkfile ();

	check_xdg ();

	test_resolve ();
	test_name ();
	test_lockname ();
	test_tempname ();


	print_result ("testmod_resolver");

	return nbError;
}
