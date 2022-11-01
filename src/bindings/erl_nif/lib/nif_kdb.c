#include <kdb.h>
#include <stdio.h>

#include "erl_nif.h"

// The default value is for an Elixir module.
#ifndef _ELEKTRA_NIF_MODULE_NAME
#define _ELEKTRA_NIF_MODULE_NAME Elixir.Elektra.System
#endif

// Needed to allow for macro expansion in the first argument due to stringification of the first argument.
#ifndef _ERL_NIF_INIT
#define _ERL_NIF_INIT(MODULE, FUNCS, LOAD, RELOAD, UPGRADE, UNLOAD) ERL_NIF_INIT (MODULE, FUNCS, LOAD, RELOAD, UPGRADE, UNLOAD)
#endif

ErlNifResourceType * KDB_RESOURCE_TYPE;
ErlNifResourceType * KEY_RESOURCE_TYPE;
ErlNifResourceType * KEY_SET_RESOURCE_TYPE;

// Destructors are called when the Erlang VM no longer holds a reference to the
// resources.

void kdb_resource_destructor (ErlNifEnv * env, void * res)
{
	printf ("kdb_resource_destructor\n");
	KDB ** handle_resource = (KDB **) res;
	kdbClose (*handle_resource, NULL);
}

void key_resource_destructor (ErlNifEnv * env, void * res)
{
	printf ("key_resource_destructor\n");
	Key ** key_resource = (Key **) res;
	keyDel (*key_resource);
}

void key_set_resource_destructor (ErlNifEnv * env, void * res)
{
	printf ("key_set_resource_destructor\n");
	KeySet ** ks_resource = (KeySet **) res;
	ksDel (*ks_resource);
}

/**************************************
 *
 * KDB methods
 *
 **************************************/

// KDB * kdbOpen (const KeySet * contract, Key *parentKey);
static ERL_NIF_TERM nif_kdb_open (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** contract_resource;
	Key ** parentKey_resource;

	printf ("kdb_open before get_resource\n");
	if (argc == 2)
	{
		printf ("two args\n");
		if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &contract_resource) ||
		    !enif_get_resource (env, argv[1], KEY_RESOURCE_TYPE, (void *) &parentKey_resource))
		{
			return enif_make_badarg (env);
		}
	}
	else
	{
		printf ("one args\n");
		if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &parentKey_resource))
		{
			return enif_make_badarg (env);
		}
		contract_resource = NULL;
	}
	printf ("after get_resource\n");

	KeySet * contract = contract_resource == NULL ? NULL : *contract_resource;
	Key * parentKey = *parentKey_resource;

	printf ("before alloc\n");
	KDB ** kdb_resource = enif_alloc_resource (KDB_RESOURCE_TYPE, sizeof (KDB *));
	printf ("after alloc\n");

	printf ("before open\n");
	KDB * kdb = kdbOpen (NULL, parentKey);
	printf ("after open\n");
	*kdb_resource = kdb;

	ERL_NIF_TERM term = enif_make_resource (env, kdb_resource);
	enif_release_resource (kdb_resource);

	return term;
}

// int kdbClose (KDB *handle, Key *errorKey);

// int kdbGet (KDB *handle, KeySet *returned, Key *parentKey);
static ERL_NIF_TERM nif_kdb_get (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KDB ** handle_resource;
	KeySet ** returned_resource;
	Key ** parentKey_resource;

	if (!enif_get_resource (env, argv[0], KDB_RESOURCE_TYPE, (void *) &handle_resource) ||
	    !enif_get_resource (env, argv[1], KEY_SET_RESOURCE_TYPE, (void *) &returned_resource) ||
	    !enif_get_resource (env, argv[2], KEY_RESOURCE_TYPE, (void *) &parentKey_resource))
	{
		enif_make_badarg (env);
	}

	KDB * handle = *handle_resource;
	KeySet * returned = *returned_resource;
	Key * parentKey = *parentKey_resource;

	int rc = kdbGet (handle, returned, parentKey);

	ERL_NIF_TERM term = enif_make_int (env, rc);

	return term;
}

// int kdbSet (KDB *handle, KeySet *returned, Key *parentKey);
static ERL_NIF_TERM nif_kdb_set (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KDB ** handle_resource;
	KeySet ** returned_resource;
	Key ** parentKey_resource;

	printf ("kdb_set before get_resource\n");

	if (!enif_get_resource (env, argv[0], KDB_RESOURCE_TYPE, (void *) &handle_resource) ||
	    !enif_get_resource (env, argv[1], KEY_SET_RESOURCE_TYPE, (void *) &returned_resource) ||
	    !enif_get_resource (env, argv[2], KEY_RESOURCE_TYPE, (void *) &parentKey_resource))
	{
		enif_make_badarg (env);
	}

	KDB * handle = *handle_resource;
	KeySet * returned = *returned_resource;
	Key * parentKey = *parentKey_resource;

	printf ("before kdbSet\n");
	int rc = kdbSet (handle, returned, parentKey);

	ERL_NIF_TERM term = enif_make_int (env, rc);

	return term;
}


static ERL_NIF_TERM nif_ks_new (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{

	KeySet ** ks_resource = enif_alloc_resource (KEY_SET_RESOURCE_TYPE, sizeof (KeySet *));

	KeySet * ks = ksNew (0, KS_END);
	*ks_resource = ks;

	ERL_NIF_TERM term = enif_make_resource (env, ks_resource);
	enif_release_resource (ks_resource);

	return term;
}

static ERL_NIF_TERM nif_ks_append_key (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** ks_resource;
	Key ** toAppend_resource;


	printf ("ks_append_key before get_resource\n");
	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &ks_resource) ||
	    !enif_get_resource (env, argv[1], KEY_RESOURCE_TYPE, (void *) &toAppend_resource))
	{
		enif_make_badarg (env);
	}

	KeySet * ks = *ks_resource;
	Key * toAppend = *toAppend_resource;

	printf ("before append\n");
	ssize_t size = ksAppendKey (ks, toAppend);
	printf ("after append\n");

	ERL_NIF_TERM term = enif_make_int (env, size);

	return term;
}

static ERL_NIF_TERM nif_key_new (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	char keyname[KEY_SIZE];
	char keyvalue[KEY_SIZE];

	printf ("argc = %d\n", argc);

	if ((argc == 2 && (enif_get_string (env, argv[0], keyname, KEY_SIZE, ERL_NIF_LATIN1) <= 0 ||
			   enif_get_string (env, argv[1], keyvalue, KEY_SIZE, ERL_NIF_LATIN1) <= 0)) ||
	    (argc == 1 && (enif_get_string (env, argv[0], keyname, KEY_SIZE, ERL_NIF_LATIN1) <= 0)))
	{
		return enif_make_badarg (env);
	}

	printf ("keyname: %s\n", keyname);
	printf ("keyvalue: %s\n", keyvalue);

	Key ** key_resource = enif_alloc_resource (KEY_RESOURCE_TYPE, sizeof (Key *));

	Key * key;

	if (argc == 2)
	{
		key = keyNew (keyname, KEY_VALUE, keyvalue, KEY_END);
	}
	else if (argc == 1)
	{
		key = keyNew (keyname, KEY_END);
	}

	*key_resource = key;

	ERL_NIF_TERM term = enif_make_resource (env, key_resource);
	enif_release_resource (key_resource);

	return term;
}

static ErlNifFunc nif_funcs[] = {
	{ "nif_ks_new", 0, nif_ks_new },     { "nif_ks_append_key", 2, nif_ks_append_key },
	{ "nif_kdb_open", 2, nif_kdb_open }, { "nif_kdb_open", 1, nif_kdb_open },
	{ "nif_kdb_get", 3, nif_kdb_get },   { "nif_kdb_set", 3, nif_kdb_set },
	{ "nif_key_new", 1, nif_key_new },   { "nif_key_new", 2, nif_key_new },
};

int load (ErlNifEnv * env, void ** priv_data, ERL_NIF_TERM load_info)
{
	int flags = ERL_NIF_RT_CREATE;

	KDB_RESOURCE_TYPE = enif_open_resource_type (env, NULL, "KDB", kdb_resource_destructor, flags, NULL);
	KEY_RESOURCE_TYPE = enif_open_resource_type (env, NULL, "Key", key_resource_destructor, flags, NULL);
	KEY_SET_RESOURCE_TYPE = enif_open_resource_type (env, NULL, "KeySet", key_set_resource_destructor, flags, NULL);

	printf ("Loading...\n");

	return 0;
}

_ERL_NIF_INIT (_ELEKTRA_NIF_MODULE_NAME, nif_funcs, load, NULL, NULL, NULL);
