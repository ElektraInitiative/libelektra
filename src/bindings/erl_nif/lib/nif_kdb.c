#include <kdb.h>

#include <stdio.h>
#include <string.h>

#include "erl_nif.h"

// Function signatures are prescribed.
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wunused-parameter"

// The default value is for an Elixir module.
#ifndef _ELEKTRA_NIF_MODULE_NAME
#define _ELEKTRA_NIF_MODULE_NAME Elixir.Elektra.System
#endif

// Needed to allow for macro expansion in the first argument due to stringification of the first argument.
#ifndef _ERL_NIF_INIT
#define _ERL_NIF_INIT(MODULE, FUNCS, LOAD, RELOAD, UPGRADE, UNLOAD) ERL_NIF_INIT (MODULE, FUNCS, LOAD, RELOAD, UPGRADE, UNLOAD)
#endif

#define _ERL_MAX_ATOM_LENGTH 1000
#define _ERL_MAX_STRING_LENGTH 1000
#define _ERL_MAX_BINARY_LENGTH 1000

ErlNifResourceType * KDB_RESOURCE_TYPE;
ErlNifResourceType * KEY_RESOURCE_TYPE;
ErlNifResourceType * KEY_SET_RESOURCE_TYPE;

/**
 * @brief Converts arbitrary data to a NIF binary.
 *
 * Arbitrary data is given in binary and copied to a binary NIF term. In
 * Erlang this is then a binary.
 *
 * @param env The NIF environment.
 * @param binary The binary data.
 * @param binary_size The size (in bytes) of the binary data.
 * @return The NIF term holding the binary data.
 */
ERL_NIF_TERM convert_binary_to_nif_binary (ErlNifEnv * env, const void * binary, size_t binary_size)
{
	ErlNifBinary * bin = malloc (sizeof (ErlNifBinary));

	enif_alloc_binary (binary_size, bin);

	memcpy (bin->data, binary, binary_size);

	ERL_NIF_TERM term = enif_make_binary (env, bin);

	enif_release_binary (bin);
	free (bin);

	return term;
}

/**
 * @brief Converts a NIF binary to a binary.
 *
 * Copies the data stored in a NIF binary into a binary.
 *
 * @param env The NIF environment.
 * @param term The NIF term storing the binary.
 * @param binary The binary where the data should be copied to.
 * @param binary_size The size (in bytes) of the allocated space for binary.
 * @return 1 on success, 0 on failure.
 */
int convert_nif_binary_to_binary (ErlNifEnv * env, ERL_NIF_TERM term, void * binary, size_t binary_size)
{
	ErlNifBinary * bin = malloc (sizeof (ErlNifBinary));

	enif_inspect_binary (env, term, bin);

	if (binary_size < bin->size)
	{
		return 0;
	}

	memcpy (binary, bin->data, bin->size);

	free (bin);

	return 1;
}

/**
 * @brief Converts a string to a NIF binary.
 *
 * Copies a string to a NIF binary without the null terminator.
 *
 * @param env The NIF environment.
 * @param string The string to copy.
 * @return The NIF term storing the string.
 */
ERL_NIF_TERM convert_string_to_nif_binary (ErlNifEnv * env, const char * string)
{
	// Null byte terminator is not copied.
	return convert_binary_to_nif_binary (env, string, strlen (string));
}

/**
 * @brief Converts a NIF binary to a string.
 *
 * Copies a string to a NIF binary without the null terminator.
 *
 * @param env The NIF environment.
 * @param term The NIF term holding the string data.
 * @param string The string where to copy to.
 * @param len The length of string.
 * @return 0 on failure, 1 on success
 */
int convert_nif_binary_to_string (ErlNifEnv * env, ERL_NIF_TERM term, char * string, size_t len)
{
	ErlNifBinary * bin = malloc (sizeof (ErlNifBinary));

	enif_inspect_binary (env, term, bin);

	if (len <= bin->size)
	{
		return 0;
	}

	memcpy (string, (char *) bin->data, bin->size);
	string[bin->size] = '\0';

	free (bin);

	return 1;
}

/**
 * @brief Check whether a NIF term is an atom with a specific value.
 *
 * @param env The NIF environment.
 * @param arg The NIF term to inspect.
 * @parm value The value which to compare the atom to.
 * @return 1 if term is an atom with value value, 0 otherwise
 */
int is_atom_with_value (ErlNifEnv * env, const ERL_NIF_TERM arg, char * value)
{
	char value_from_atom[_ERL_MAX_ATOM_LENGTH];

	int rc = enif_get_atom (env, arg, value_from_atom, _ERL_MAX_ATOM_LENGTH, ERL_NIF_LATIN1);

	if (!rc)
	{
		return 0;
	}

	return strncmp (value, value_from_atom, _ERL_MAX_ATOM_LENGTH) == 0;
}

/**
 * @brief Check whether a NIF term is the "null" atom.
 *
 * The atom "null" is used when one wishes to pass "NULL" as an argument to a
 * function.
 *
 * @param env The NIF environment.
 * @param arg The NIF term to inspect.
 * @return 1 if term is the "null" atom, 0 otherwise
 */
int is_null_atom (ErlNifEnv * env, const ERL_NIF_TERM arg)
{
	return is_atom_with_value (env, arg, "null");
}


/**************************************
 *
 * KDB methods
 *
 **************************************/

// KDB * kdbOpen (const KeySet * contract, Key *parentKey);
static ERL_NIF_TERM kdb_open (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** contract_resource;
	Key ** parentKey_resource;

	if (is_null_atom (env, argv[0]))
	{
		contract_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &contract_resource))
	{
		return enif_make_badarg (env);
	}
	if (is_null_atom (env, argv[1]))
	{
		parentKey_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[1], KEY_RESOURCE_TYPE, (void *) &parentKey_resource))
	{
		return enif_make_badarg (env);
	}

	KeySet * contract = contract_resource == NULL ? NULL : *contract_resource;
	Key * parentKey = parentKey_resource == NULL ? NULL : *parentKey_resource;

	KDB ** kdb_resource = enif_alloc_resource (KDB_RESOURCE_TYPE, sizeof (KDB *));

	KDB * kdb = kdbOpen (contract, parentKey);

	*kdb_resource = kdb;

	ERL_NIF_TERM term = enif_make_resource (env, kdb_resource);
	enif_release_resource (kdb_resource);

	return term;
}

// int kdbClose (KDB *handle, Key *errorKey);
static ERL_NIF_TERM kdb_close (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KDB ** handle_resource;
	Key ** errorKey_resource;

	if (is_null_atom (env, argv[0]))
	{
		handle_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[0], KDB_RESOURCE_TYPE, (void *) &handle_resource))
	{
		return enif_make_badarg (env);
	}
	if (is_null_atom (env, argv[1]))
	{
		errorKey_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[1], KEY_RESOURCE_TYPE, (void *) &errorKey_resource))
	{
		return enif_make_badarg (env);
	}

	KDB * handle = handle_resource == NULL ? NULL : *handle_resource;
	Key * errorKey = errorKey_resource == NULL ? NULL : *errorKey_resource;

	int rc = kdbClose (handle, errorKey);
	ERL_NIF_TERM term = enif_make_int (env, rc);

	return term;
}

// int kdbGet (KDB *handle, KeySet *returned, Key *parentKey);
static ERL_NIF_TERM kdb_get (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KDB ** handle_resource;
	KeySet ** returned_resource;
	Key ** parentKey_resource;

	if (is_null_atom (env, argv[0]))
	{
		handle_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[0], KDB_RESOURCE_TYPE, (void *) &handle_resource))
	{
		return enif_make_badarg (env);
	}
	if (is_null_atom (env, argv[1]))
	{
		returned_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[1], KEY_SET_RESOURCE_TYPE, (void *) &returned_resource))
	{
		return enif_make_badarg (env);
	}
	if (is_null_atom (env, argv[2]))
	{
		parentKey_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[2], KEY_RESOURCE_TYPE, (void *) &parentKey_resource))
	{
		return enif_make_badarg (env);
	}

	KDB * handle = handle_resource == NULL ? NULL : *handle_resource;
	KeySet * returned = returned_resource == NULL ? NULL : *returned_resource;
	Key * parentKey = parentKey_resource == NULL ? NULL : *parentKey_resource;

	int rc = kdbGet (handle, returned, parentKey);

	ERL_NIF_TERM term = enif_make_int (env, rc);

	return term;
}

// int kdbSet (KDB *handle, KeySet *returned, Key *parentKey);
static ERL_NIF_TERM kdb_set (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KDB ** handle_resource;
	KeySet ** returned_resource;
	Key ** parentKey_resource;

	if (is_null_atom (env, argv[0]))
	{
		handle_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[0], KDB_RESOURCE_TYPE, (void *) &handle_resource))
	{
		return enif_make_badarg (env);
	}
	if (is_null_atom (env, argv[1]))
	{
		returned_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[1], KEY_SET_RESOURCE_TYPE, (void *) &returned_resource))
	{
		return enif_make_badarg (env);
	}
	if (is_null_atom (env, argv[2]))
	{
		parentKey_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[2], KEY_RESOURCE_TYPE, (void *) &parentKey_resource))
	{
		return enif_make_badarg (env);
	}

	KDB * handle = handle_resource == NULL ? NULL : *handle_resource;
	KeySet * returned = returned_resource == NULL ? NULL : *returned_resource;
	Key * parentKey = parentKey_resource == NULL ? NULL : *parentKey_resource;

	int rc = kdbSet (handle, returned, parentKey);

	ERL_NIF_TERM term = enif_make_int (env, rc);

	return term;
}


/**************************************
 *
 * Key methods
 *
 **************************************/

// Key *keyNew (const char *keyname, ...) ELEKTRA_SENTINEL;
static ERL_NIF_TERM key_new (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	char name[_ERL_MAX_STRING_LENGTH];

	if (!convert_nif_binary_to_string (env, argv[0], name, _ERL_MAX_STRING_LENGTH))
	{
		return enif_make_badarg (env);
	}

	Key ** key_resource = enif_alloc_resource (KEY_RESOURCE_TYPE, sizeof (Key *));

	Key * key = keyNew (name, KEY_END);

	*key_resource = key;

	ERL_NIF_TERM term = enif_make_resource (env, key_resource);
	enif_release_resource (key_resource);

	return term;
}

// Key * keyCopy (Key *dest, const Key *source, elektraCopyFlags flags);
static ERL_NIF_TERM key_copy (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** dest_resource;
	Key ** source_resource;
	elektraCopyFlags flags;

	if (is_null_atom (env, argv[0]))
	{
		dest_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &dest_resource))
	{
		return enif_make_badarg (env);
	}
	if (is_null_atom (env, argv[1]))
	{
		source_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[1], KEY_RESOURCE_TYPE, (void *) &source_resource))
	{
		return enif_make_badarg (env);
	}
	if (!enif_get_uint (env, argv[2], &flags))
	{
		return enif_make_badarg (env);
	}

	Key * dest = dest_resource == NULL ? NULL : *dest_resource;
	Key * source = source_resource == NULL ? NULL : *source_resource;

	Key * result = keyCopy (dest, source, flags);

	Key ** result_resource = enif_alloc_resource (KEY_RESOURCE_TYPE, sizeof (Key *));
	*result_resource = result;

	ERL_NIF_TERM term = enif_make_resource (env, result_resource);
	enif_release_resource (result_resource);

	return term;
}

// int keyClear (Key *key);
static ERL_NIF_TERM key_clear (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = *key_resource;

	int result = keyClear (key);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// int keyDel (Key *key);
static ERL_NIF_TERM key_del (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = *key_resource;

	int result = keyDel (key);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// uint16_t keyIncRef (Key *key);
static ERL_NIF_TERM key_inc_ref (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = *key_resource;

	uint16_t result = keyIncRef (key);

	ERL_NIF_TERM term = enif_make_uint (env, result);

	return term;
}

// uint16_t keyDecRef (Key *key);
static ERL_NIF_TERM key_dec_ref (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = *key_resource;

	uint16_t result = keyDecRef (key);

	ERL_NIF_TERM term = enif_make_uint (env, result);

	return term;
}

// uint16_t keyGetRef (const Key *key);
static ERL_NIF_TERM key_get_ref (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = *key_resource;

	uint16_t result = keyGetRef (key);

	ERL_NIF_TERM term = enif_make_uint (env, result);

	return term;
}

// int keyCopyMeta (Key *dest, const Key *source, const char *metaName);
static ERL_NIF_TERM key_copy_meta (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** dest_resource;
	Key ** source_resource;
	char metaName[_ERL_MAX_STRING_LENGTH];

	if (is_null_atom (env, argv[0]))
	{
		dest_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &dest_resource))
	{
		return enif_make_badarg (env);
	}
	if (is_null_atom (env, argv[1]))
	{
		source_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[1], KEY_RESOURCE_TYPE, (void *) &source_resource))
	{
		return enif_make_badarg (env);
	}
	if (!convert_nif_binary_to_string (env, argv[2], metaName, _ERL_MAX_STRING_LENGTH))
	{
		return enif_make_badarg (env);
	}

	Key * dest = dest_resource == NULL ? NULL : *dest_resource;
	Key * source = source_resource == NULL ? NULL : *source_resource;

	int result = keyCopyMeta (dest, source, metaName);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// int keyCopyAllMeta (Key *dest, const Key *source);
static ERL_NIF_TERM key_copy_all_meta (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** dest_resource;
	Key ** source_resource;

	if (is_null_atom (env, argv[0]))
	{
		dest_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &dest_resource))
	{
		return enif_make_badarg (env);
	}
	if (is_null_atom (env, argv[1]))
	{
		source_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[1], KEY_RESOURCE_TYPE, (void *) &source_resource))
	{
		return enif_make_badarg (env);
	}

	Key * dest = dest_resource == NULL ? NULL : *dest_resource;
	Key * source = source_resource == NULL ? NULL : *source_resource;

	int result = keyCopyAllMeta (dest, source);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// const Key *keyGetMeta (const Key *key, const char* metaName);
static ERL_NIF_TERM key_get_meta (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;
	char metaName[_ERL_MAX_STRING_LENGTH];

	if (is_null_atom (env, argv[0]))
	{
		key_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}
	if (!convert_nif_binary_to_string (env, argv[1], metaName, _ERL_MAX_STRING_LENGTH))
	{
		return enif_make_badarg (env);
	}

	Key * key = key_resource == NULL ? NULL : *key_resource;

	const Key * result = keyGetMeta (key, metaName);

	const Key ** result_resource = enif_alloc_resource (KEY_RESOURCE_TYPE, sizeof (Key *));
	*result_resource = result;

	ERL_NIF_TERM term = enif_make_resource (env, result_resource);
	enif_release_resource (result_resource);

	return term;
}

// ssize_t keySetMeta (Key *key, const char* metaName, const char *newMetaString);
static ERL_NIF_TERM key_set_meta (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;
	char metaName[_ERL_MAX_STRING_LENGTH];
	char newMetaString[_ERL_MAX_STRING_LENGTH];

	if (is_null_atom (env, argv[0]))
	{
		key_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}
	if (!convert_nif_binary_to_string (env, argv[1], metaName, _ERL_MAX_STRING_LENGTH))
	{
		return enif_make_badarg (env);
	}
	if (!convert_nif_binary_to_string (env, argv[2], newMetaString, _ERL_MAX_STRING_LENGTH))
	{
		return enif_make_badarg (env);
	}

	Key * key = key_resource == NULL ? NULL : *key_resource;

	unsigned result = keySetMeta (key, metaName, newMetaString);

	ERL_NIF_TERM term = enif_make_uint (env, result);

	return term;
}

// KeySet * keyMeta (Key * key);
static ERL_NIF_TERM key_meta (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;

	if (is_null_atom (env, argv[0]))
	{
		key_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = key_resource == NULL ? NULL : *key_resource;

	KeySet * result = keyMeta (key);

	KeySet ** result_resource = enif_alloc_resource (KEY_SET_RESOURCE_TYPE, sizeof (KeySet *));
	*result_resource = result;

	ERL_NIF_TERM term = enif_make_resource (env, result_resource);
	enif_release_resource (result_resource);

	return term;
}

// int keyCmp (const Key *k1, const Key *k2);
static ERL_NIF_TERM key_cmp (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** k1_resource;
	Key ** k2_resource;

	if (is_null_atom (env, argv[0]))
	{
		k1_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &k1_resource))
	{
		return enif_make_badarg (env);
	}
	if (is_null_atom (env, argv[1]))
	{
		k2_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[1], KEY_RESOURCE_TYPE, (void *) &k2_resource))
	{
		return enif_make_badarg (env);
	}

	Key * k1 = k1_resource == NULL ? NULL : *k1_resource;
	Key * k2 = k2_resource == NULL ? NULL : *k2_resource;

	int result = keyCmp (k1, k2);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// int keyNeedSync (const Key *key);
static ERL_NIF_TERM key_need_sync (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;

	if (is_null_atom (env, argv[0]))
	{
		key_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = key_resource == NULL ? NULL : *key_resource;

	int result = keyNeedSync (key);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// int keyIsBelow (const Key *key, const Key *check);
static ERL_NIF_TERM key_is_below (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;
	Key ** check_resource;

	if (is_null_atom (env, argv[0]))
	{
		key_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}
	if (is_null_atom (env, argv[1]))
	{
		check_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[1], KEY_RESOURCE_TYPE, (void *) &check_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = key_resource == NULL ? NULL : *key_resource;
	Key * check = check_resource == NULL ? NULL : *check_resource;

	int result = keyIsBelow (key, check);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// int keyIsBelowOrSame (const Key *key, const Key *check);
static ERL_NIF_TERM key_is_below_or_same (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;
	Key ** check_resource;

	if (is_null_atom (env, argv[0]))
	{
		key_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}
	if (is_null_atom (env, argv[1]))
	{
		check_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[1], KEY_RESOURCE_TYPE, (void *) &check_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = key_resource == NULL ? NULL : *key_resource;
	Key * check = check_resource == NULL ? NULL : *check_resource;

	int result = keyIsBelowOrSame (key, check);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// int keyIsDirectlyBelow (const Key *key, const Key *check);
static ERL_NIF_TERM key_is_directly_below (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;
	Key ** check_resource;

	if (is_null_atom (env, argv[0]))
	{
		key_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}
	if (is_null_atom (env, argv[1]))
	{
		check_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[1], KEY_RESOURCE_TYPE, (void *) &check_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = key_resource == NULL ? NULL : *key_resource;
	Key * check = check_resource == NULL ? NULL : *check_resource;

	int result = keyIsDirectlyBelow (key, check);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// int keyIsBinary (const Key *key);
static ERL_NIF_TERM key_is_binary (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;

	if (is_null_atom (env, argv[0]))
	{
		key_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = key_resource == NULL ? NULL : *key_resource;

	int result = keyIsBinary (key);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// int keyIsString (const Key *key);
static ERL_NIF_TERM key_is_string (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;

	if (is_null_atom (env, argv[0]))
	{
		key_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = key_resource == NULL ? NULL : *key_resource;

	int result = keyIsString (key);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// const char *keyName (const Key *key);
static ERL_NIF_TERM key_name (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;

	if (is_null_atom (env, argv[0]))
	{
		key_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = key_resource == NULL ? NULL : *key_resource;

	const char * result = keyName (key);

	ERL_NIF_TERM term = convert_string_to_nif_binary (env, result);

	return term;
}

// ssize_t keyGetNameSize (const Key *key);
static ERL_NIF_TERM key_get_name_size (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;

	if (is_null_atom (env, argv[0]))
	{
		key_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = key_resource == NULL ? NULL : *key_resource;

	unsigned result = keyGetNameSize (key);

	ERL_NIF_TERM term = enif_make_uint (env, result);

	return term;
}

// ssize_t keySetName (Key *key, const char *newname);
static ERL_NIF_TERM key_set_name (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;
	char newname[_ERL_MAX_STRING_LENGTH];

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}
	if (!convert_nif_binary_to_string (env, argv[1], newname, _ERL_MAX_STRING_LENGTH))
	{
		return enif_make_badarg (env);
	}

	Key * key = *key_resource;

	ssize_t result = keySetName (key, newname);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// ssize_t keyAddName (Key *key, const char *addName);
static ERL_NIF_TERM key_add_name (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;
	char addName[_ERL_MAX_STRING_LENGTH];

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}
	if (!convert_nif_binary_to_string (env, argv[1], addName, _ERL_MAX_STRING_LENGTH))
	{
		return enif_make_badarg (env);
	}

	Key * key = *key_resource;

	ssize_t result = keyAddName (key, addName);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// const void *keyUnescapedName (const Key *key);
static ERL_NIF_TERM key_unescaped_name (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = *key_resource;

	size_t size = keyGetUnescapedNameSize (key);

	char buffer[size];
	const char * unescapedName = keyUnescapedName (key);
	memcpy (buffer, unescapedName, size);

	ERL_NIF_TERM term = convert_binary_to_nif_binary (env, buffer, size);

	return term;
}

// ssize_t keyGetUnescapedNameSize (const Key *key);
static ERL_NIF_TERM key_get_unescaped_name_size (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = *key_resource;

	size_t size = keyGetUnescapedNameSize (key);

	ERL_NIF_TERM term = enif_make_uint64 (env, size);

	return term;
}

// const char *keyBaseName (const Key *key);
static ERL_NIF_TERM key_base_name (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;

	if (is_null_atom (env, argv[0]))
	{
		key_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = key_resource == NULL ? NULL : *key_resource;

	const char * result = keyBaseName (key);

	ERL_NIF_TERM term = convert_string_to_nif_binary (env, result);

	return term;
}

// ssize_t keyGetBaseNameSize (const Key *key);
static ERL_NIF_TERM key_get_base_name_size (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = *key_resource;

	size_t size = keyGetBaseNameSize (key);

	ERL_NIF_TERM term = enif_make_uint64 (env, size);

	return term;
}

// ssize_t keySetBaseName (Key *key,const char *baseName);
static ERL_NIF_TERM key_set_base_name (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;
	char baseName[_ERL_MAX_STRING_LENGTH];

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}
	if (!convert_nif_binary_to_string (env, argv[1], baseName, _ERL_MAX_STRING_LENGTH))
	{
		return enif_make_badarg (env);
	}

	Key * key = *key_resource;

	ssize_t result = keySetBaseName (key, baseName);

	ERL_NIF_TERM term = enif_make_int64 (env, result);

	return term;
}

// ssize_t keyAddBaseName (Key *key,const char *baseName);
static ERL_NIF_TERM key_add_base_name (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;
	char baseName[_ERL_MAX_STRING_LENGTH];

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}
	if (!convert_nif_binary_to_string (env, argv[1], baseName, _ERL_MAX_STRING_LENGTH))
	{
		return enif_make_badarg (env);
	}

	Key * key = *key_resource;

	ssize_t result = keyAddBaseName (key, baseName);

	ERL_NIF_TERM term = enif_make_int64 (env, result);

	return term;
}

// elektraNamespace keyGetNamespace (Key const* key);
static ERL_NIF_TERM key_get_namespace (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = *key_resource;

	int result = keyGetNamespace (key);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// ssize_t keySetNamespace (Key * key, elektraNamespace ns);
static ERL_NIF_TERM key_set_namespace (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;
	int ns;

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}
	if (!enif_get_int (env, argv[1], &ns))
	{
		return enif_make_badarg (env);
	}

	Key * key = *key_resource;

	int result = keySetNamespace (key, ns);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// const void *keyValue (const Key *key);
static ERL_NIF_TERM key_value (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = *key_resource;

	size_t size = keyGetValueSize (key);
	const void * value = keyValue (key);

	ERL_NIF_TERM term = convert_binary_to_nif_binary (env, value, size);

	return term;
}

// ssize_t keyGetValueSize (const Key *key);
static ERL_NIF_TERM key_get_value_size (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = *key_resource;

	size_t size = keyGetValueSize (key);

	ERL_NIF_TERM term = enif_make_uint64 (env, size);

	return term;
}

// const char *keyString (const Key *key);
static ERL_NIF_TERM key_string (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;

	if (is_null_atom (env, argv[0]))
	{
		key_resource = NULL;
	}
	else if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = key_resource == NULL ? NULL : *key_resource;

	const char * result = keyString (key);

	ERL_NIF_TERM term = convert_string_to_nif_binary (env, result);

	return term;
}

// ssize_t keySetString (Key *key, const char *newString);
static ERL_NIF_TERM key_set_string (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;
	char newString[_ERL_MAX_STRING_LENGTH];

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}
	if (!convert_nif_binary_to_string (env, argv[1], newString, _ERL_MAX_STRING_LENGTH))
	{
		return enif_make_badarg (env);
	}

	Key * key = *key_resource;

	ssize_t result = keySetString (key, newString);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// ssize_t keyGetBinary (const Key *key, void *returnedBinary, size_t maxSize);
static ERL_NIF_TERM key_get_binary (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}

	Key * key = *key_resource;

	size_t size = keyGetValueSize (key);
	char returnedBinary[size];
	keyGetBinary (key, returnedBinary, size);
	// TODO: Handle error

	ERL_NIF_TERM term = convert_binary_to_nif_binary (env, returnedBinary, size);

	return term;
}

// ssize_t keySetBinary (Key *key, const void *newBinary, size_t dataSize);
static ERL_NIF_TERM key_set_binary (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;
	char newBinary[_ERL_MAX_BINARY_LENGTH];
	size_t dataSize;

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}
	if (!convert_nif_binary_to_binary (env, argv[1], newBinary, _ERL_MAX_STRING_LENGTH))
	{
		return enif_make_badarg (env);
	}
	if (!enif_get_uint64 (env, argv[2], &dataSize))
	{
		return enif_make_badarg (env);
	}

	Key * key = *key_resource;

	ssize_t result = keySetBinary (key, newBinary, dataSize);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// int keyLock (Key * key, elektraLockFlags what);
static ERL_NIF_TERM key_lock (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;
	elektraLockFlags what;

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}
	if (!enif_get_int (env, argv[1], &what))
	{
		return enif_make_badarg (env);
	}

	Key * key = *key_resource;

	int result = keyLock (key, what);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}
// int keyIsLocked (const Key * key, elektraLockFlags what);
static ERL_NIF_TERM key_is_locked (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** key_resource;
	elektraLockFlags what;

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}
	if (!enif_get_int (env, argv[1], &what))
	{
		return enif_make_badarg (env);
	}

	Key * key = *key_resource;

	int result = keyIsLocked (key, what);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// static inline Key *keyDup (const Key *source, elektraCopyFlags flags)
// {
//	return keyCopy (keyNew ("/", KEY_END), source, flags);
// }
static ERL_NIF_TERM key_dup (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	Key ** source_resource;
	elektraCopyFlags flags;

	if (!enif_get_resource (env, argv[0], KEY_RESOURCE_TYPE, (void *) &source_resource))
	{
		return enif_make_badarg (env);
	}
	if (!enif_get_uint (env, argv[1], &flags))
	{
		return enif_make_badarg (env);
	}

	Key * source = *source_resource;

	Key * copy = keyDup (source, flags);

	Key ** copy_resource = enif_alloc_resource (KEY_RESOURCE_TYPE, sizeof (Key *));
	*copy_resource = copy;

	ERL_NIF_TERM term = enif_make_resource (env, copy_resource);
	enif_release_resource (copy_resource);

	return term;
}

/**************************************
 *
 * KeySet methods
 *
 **************************************/

// KeySet * ksNew (size_t alloc, ...) ELEKTRA_SENTINEL;
static ERL_NIF_TERM ks_new (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	size_t alloc;
	if (!enif_get_uint64 (env, argv[0], &alloc))
	{
		return enif_make_badarg (env);
	}

	KeySet ** ks_resource = enif_alloc_resource (KEY_SET_RESOURCE_TYPE, sizeof (KeySet *));

	KeySet * ks = ksNew (alloc, KS_END);

	*ks_resource = ks;

	ERL_NIF_TERM term = enif_make_resource (env, ks_resource);
	enif_release_resource (ks_resource);

	return term;
}

// KeySet * ksDup (const KeySet * source);
static ERL_NIF_TERM ks_dup (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** source_resource;

	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &source_resource))
	{
		return enif_make_badarg (env);
	}

	KeySet * source = *source_resource;

	KeySet * copy = ksDup (source);

	KeySet ** copy_resource = enif_alloc_resource (KEY_SET_RESOURCE_TYPE, sizeof (KeySet *));
	*copy_resource = copy;

	ERL_NIF_TERM term = enif_make_resource (env, copy_resource);
	enif_release_resource (copy_resource);

	return term;
}

// int ksCopy (KeySet * dest, const KeySet * source);
static ERL_NIF_TERM ks_copy (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** dest_resource;
	KeySet ** source_resource;

	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &dest_resource))
	{
		return enif_make_badarg (env);
	}
	if (!enif_get_resource (env, argv[1], KEY_SET_RESOURCE_TYPE, (void *) &source_resource))
	{
		return enif_make_badarg (env);
	}

	KeySet * dest = *dest_resource;
	KeySet * source = *source_resource;

	int result = ksCopy (dest, source);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// uint16_t ksIncRef (KeySet * ks);
static ERL_NIF_TERM ks_inc_ref (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** ks_resource;

	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &ks_resource))
	{
		return enif_make_badarg (env);
	}

	KeySet * ks = *ks_resource;

	uint16_t result = ksIncRef (ks);

	ERL_NIF_TERM term = enif_make_uint (env, result);

	return term;
}

// uint16_t ksDecRef (KeySet * ks);
static ERL_NIF_TERM ks_dec_ref (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** ks_resource;

	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &ks_resource))
	{
		return enif_make_badarg (env);
	}

	KeySet * ks = *ks_resource;

	uint16_t result = ksDecRef (ks);

	ERL_NIF_TERM term = enif_make_uint (env, result);

	return term;
}

// uint16_t ksGetRef (const KeySet * ks);
static ERL_NIF_TERM ks_get_ref (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** ks_resource;

	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &ks_resource))
	{
		return enif_make_badarg (env);
	}

	KeySet * ks = *ks_resource;

	uint16_t result = ksIncRef (ks);

	ERL_NIF_TERM term = enif_make_uint (env, result);

	return term;
}

// int ksClear (KeySet * ks);
static ERL_NIF_TERM ks_clear (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** ks_resource;

	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &ks_resource))
	{
		return enif_make_badarg (env);
	}

	KeySet * ks = *ks_resource;

	int result = ksClear (ks);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// int ksDel (KeySet * ks);
static ERL_NIF_TERM ks_del (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** ks_resource;

	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &ks_resource))
	{
		return enif_make_badarg (env);
	}

	KeySet * ks = *ks_resource;

	int result = ksDel (ks);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// ssize_t ksGetSize (const KeySet * ks);
static ERL_NIF_TERM ks_get_size (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** ks_resource;

	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &ks_resource))
	{
		return enif_make_badarg (env);
	}

	KeySet * ks = *ks_resource;

	ssize_t result = ksGetSize (ks);

	ERL_NIF_TERM term = enif_make_uint64 (env, result);

	return term;
}

// ssize_t ksAppendKey (KeySet * ks, Key * toAppend);
static ERL_NIF_TERM ks_append_key (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** ks_resource;
	Key ** toAppend_resource;

	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &ks_resource))
	{
		return enif_make_badarg (env);
	}
	if (!enif_get_resource (env, argv[1], KEY_RESOURCE_TYPE, (void *) &toAppend_resource))
	{
		return enif_make_badarg (env);
	}

	KeySet * ks = *ks_resource;
	Key * toAppend = *toAppend_resource;

	ssize_t result = ksAppendKey (ks, toAppend);

	ERL_NIF_TERM term = enif_make_uint64 (env, result);

	return term;
}

// ssize_t ksAppend (KeySet * ks, const KeySet * toAppend);
static ERL_NIF_TERM ks_append (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** ks_resource;
	KeySet ** toAppend_resource;

	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &ks_resource))
	{
		return enif_make_badarg (env);
	}
	if (!enif_get_resource (env, argv[1], KEY_SET_RESOURCE_TYPE, (void *) &toAppend_resource))
	{
		return enif_make_badarg (env);
	}

	KeySet * ks = *ks_resource;
	KeySet * toAppend = *toAppend_resource;

	ssize_t result = ksAppend (ks, toAppend);

	ERL_NIF_TERM term = enif_make_uint64 (env, result);

	return term;
}

// KeySet * ksCut (KeySet * ks, const Key * cutpoint);
static ERL_NIF_TERM ks_cut (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** ks_resource;
	Key ** cutpoint_resource;

	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &ks_resource))
	{
		return enif_make_badarg (env);
	}
	if (!enif_get_resource (env, argv[1], KEY_RESOURCE_TYPE, (void *) &cutpoint_resource))
	{
		return enif_make_badarg (env);
	}

	KeySet * ks = *ks_resource;
	Key * cutpoint = *cutpoint_resource;

	KeySet * result = ksCut (ks, cutpoint);

	KeySet ** result_resource = enif_alloc_resource (KEY_SET_RESOURCE_TYPE, sizeof (KeySet *));
	*result_resource = result;

	ERL_NIF_TERM term = enif_make_resource (env, result_resource);
	enif_release_resource (result_resource);

	return term;
}

// Key * ksPop (KeySet * ks);
static ERL_NIF_TERM ks_pop (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** ks_resource;

	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &ks_resource))
	{
		return enif_make_badarg (env);
	}

	KeySet * ks = *ks_resource;

	Key * result = ksPop (ks);

	Key ** result_resource = enif_alloc_resource (KEY_RESOURCE_TYPE, sizeof (Key *));
	*result_resource = result;

	ERL_NIF_TERM term = enif_make_resource (env, result_resource);
	enif_release_resource (result_resource);

	return term;
}

// int ksRewind (KeySet * ks);
static ERL_NIF_TERM ks_rewind (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** ks_resource;

	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &ks_resource))
	{
		return enif_make_badarg (env);
	}

	KeySet * ks = *ks_resource;

	int result = ksRewind (ks);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// Key * ksNext (KeySet * ks);
static ERL_NIF_TERM ks_next (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** ks_resource;

	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &ks_resource))
	{
		return enif_make_badarg (env);
	}

	KeySet * ks = *ks_resource;

	Key * result = ksNext (ks);

	Key ** result_resource = enif_alloc_resource (KEY_RESOURCE_TYPE, sizeof (Key *));
	*result_resource = result;

	ERL_NIF_TERM term = enif_make_resource (env, result_resource);
	enif_release_resource (result_resource);

	return term;
}

// Key * ksCurrent (const KeySet * ks);
static ERL_NIF_TERM ks_current (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** ks_resource;

	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &ks_resource))
	{
		return enif_make_badarg (env);
	}

	KeySet * ks = *ks_resource;

	Key * result = ksCurrent (ks);

	Key ** result_resource = enif_alloc_resource (KEY_RESOURCE_TYPE, sizeof (Key *));
	*result_resource = result;

	ERL_NIF_TERM term = enif_make_resource (env, result_resource);
	enif_release_resource (result_resource);

	return term;
}

// elektraCursor ksGetCursor (const KeySet * ks);
static ERL_NIF_TERM ks_get_cursor (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** ks_resource;

	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &ks_resource))
	{
		return enif_make_badarg (env);
	}

	KeySet * ks = *ks_resource;

	elektraCursor result = ksGetCursor (ks);

	ERL_NIF_TERM term = enif_make_int64 (env, result);

	return term;
}

// int ksSetCursor (KeySet * ks, elektraCursor cursor);
static ERL_NIF_TERM ks_set_cursor (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** ks_resource;
	elektraCursor cursor;

	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &ks_resource))
	{
		return enif_make_badarg (env);
	}
	if (!enif_get_int64 (env, argv[1], &cursor))
	{
		return enif_make_badarg (env);
	}

	KeySet * ks = *ks_resource;

	int result = ksSetCursor (ks, cursor);

	ERL_NIF_TERM term = enif_make_int (env, result);

	return term;
}

// Key * ksAtCursor (const KeySet * ks, elektraCursor cursor);
static ERL_NIF_TERM ks_at_cursor (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** ks_resource;
	elektraCursor cursor;

	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &ks_resource))
	{
		return enif_make_badarg (env);
	}
	if (!enif_get_int64 (env, argv[1], &cursor))
	{
		return enif_make_badarg (env);
	}

	KeySet * ks = *ks_resource;

	Key * result = ksAtCursor (ks, cursor);

	Key ** result_resource = enif_alloc_resource (KEY_RESOURCE_TYPE, sizeof (Key *));
	*result_resource = result;

	ERL_NIF_TERM term = enif_make_resource (env, result_resource);
	enif_release_resource (result_resource);

	return term;
}

// Key * ksLookup (KeySet * ks, Key * k, elektraLookupFlags options);
static ERL_NIF_TERM ks_lookup (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** ks_resource;
	Key ** k_resource;
	elektraLookupFlags options;

	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &ks_resource))
	{
		return enif_make_badarg (env);
	}
	if (!enif_get_resource (env, argv[1], KEY_RESOURCE_TYPE, (void *) &k_resource))
	{
		return enif_make_badarg (env);
	}
	if (!enif_get_int (env, argv[2], &options))
	{
		return enif_make_badarg (env);
	}

	KeySet * ks = *ks_resource;
	Key * k = *k_resource;

	Key * result = ksLookup (ks, k, options);

	Key ** result_resource = enif_alloc_resource (KEY_RESOURCE_TYPE, sizeof (Key *));
	*result_resource = result;

	ERL_NIF_TERM term = enif_make_resource (env, result_resource);
	enif_release_resource (result_resource);

	return term;
}

// Key * ksLookupByName (KeySet * ks, const char * name, elektraLookupFlags options);
static ERL_NIF_TERM ks_lookup_by_name (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** ks_resource;
	char name[_ERL_MAX_STRING_LENGTH];
	elektraLookupFlags options;

	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &ks_resource))
	{
		return enif_make_badarg (env);
	}
	if (!convert_nif_binary_to_string (env, argv[1], name, _ERL_MAX_STRING_LENGTH))
	{
		return enif_make_badarg (env);
	}
	if (!enif_get_int (env, argv[2], &options))
	{
		return enif_make_badarg (env);
	}

	KeySet * ks = *ks_resource;

	Key * result = ksLookupByName (ks, name, options);

	Key ** result_resource = enif_alloc_resource (KEY_RESOURCE_TYPE, sizeof (Key *));
	*result_resource = result;

	ERL_NIF_TERM term = enif_make_resource (env, result_resource);
	enif_release_resource (result_resource);

	return term;
}

// ssize_t ksSearch (const KeySet * ks, const Key * key);
static ERL_NIF_TERM ks_search (ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[])
{
	KeySet ** ks_resource;
	Key ** key_resource;

	if (!enif_get_resource (env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &ks_resource))
	{
		return enif_make_badarg (env);
	}
	if (!enif_get_resource (env, argv[1], KEY_RESOURCE_TYPE, (void *) &key_resource))
	{
		return enif_make_badarg (env);
	}

	KeySet * ks = *ks_resource;
	Key * key = *key_resource;

	ssize_t result = ksSearch (ks, key);

	ERL_NIF_TERM term = enif_make_int64 (env, result);

	return term;
}

// clang-format off
static ErlNifFunc funcs[] = {
	{"kdb_open", 2, kdb_open, 0},
	{"kdb_close", 2, kdb_close, 0},
	{"kdb_get", 3, kdb_get, 0},
	{"kdb_set", 3, kdb_set, 0},
	{"key_new", 1, key_new, 0},
	{"key_copy", 3, key_copy, 0},
	{"key_clear", 1, key_clear, 0},
	{"key_del", 1, key_del, 0},
	{"key_inc_ref", 1, key_inc_ref, 0},
	{"key_dec_ref", 1, key_dec_ref, 0},
	{"key_get_ref", 1, key_get_ref, 0},
	{"key_copy_meta", 3, key_copy_meta, 0},
	{"key_copy_all_meta", 2, key_copy_all_meta, 0},
	{"key_get_meta", 2, key_get_meta, 0},
	{"key_set_meta", 3, key_set_meta, 0},
	{"key_meta", 1, key_meta, 0},
	{"key_cmp", 2, key_cmp, 0},
	{"key_need_sync", 1, key_need_sync, 0},
	{"key_is_below", 2, key_is_below, 0},
	{"key_is_below_or_same", 2, key_is_below_or_same, 0},
	{"key_is_directly_below", 2, key_is_directly_below, 0},
	{"key_is_binary", 1, key_is_binary, 0},
	{"key_is_string", 1, key_is_string, 0},
	{"key_name", 1, key_name, 0},
	{"key_get_name_size", 1, key_get_name_size, 0},
	{"key_set_name", 2, key_set_name, 0},
	{"key_add_name", 2, key_add_name, 0},
	{"key_unescaped_name", 1, key_unescaped_name, 0},
	{"key_get_unescaped_name_size", 1, key_get_unescaped_name_size, 0},
	{"key_base_name", 1, key_base_name, 0},
	{"key_get_base_name_size", 1, key_get_base_name_size, 0},
	{"key_set_base_name", 2, key_set_base_name, 0},
	{"key_add_base_name", 2, key_add_base_name, 0},
	{"key_get_namespace", 1, key_get_namespace, 0},
	{"key_set_namespace", 2, key_set_namespace, 0},
	{"key_value", 1, key_value, 0},
	{"key_get_value_size", 1, key_get_value_size, 0},
	{"key_string", 1, key_string, 0},
	{"key_set_string", 2, key_set_string, 0},
	{"key_get_binary", 1, key_get_binary, 0},
	{"key_set_binary", 2, key_set_binary, 0},
	{"key_lock", 2, key_lock, 0},
	{"key_is_locked", 1, key_is_locked, 0},
	{"key_dup", 2, key_dup, 0},
	{"ks_new", 1, ks_new, 0},
	{"ks_dup", 1, ks_dup, 0},
	{"ks_copy", 2, ks_copy, 0},
	{"ks_inc_ref", 1, ks_inc_ref, 0},
	{"ks_dec_ref", 1, ks_dec_ref, 0},
	{"ks_get_ref", 1, ks_get_ref, 0},
	{"ks_clear", 1, ks_clear, 0},
	{"ks_del", 1, ks_del, 0},
	{"ks_get_size", 1, ks_get_size, 0},
	{"ks_append_key", 2, ks_append_key, 0},
	{"ks_append", 2, ks_append, 0},
	{"ks_cut", 2, ks_cut, 0},
	{"ks_pop", 1, ks_pop, 0},
	{"ks_rewind", 1, ks_rewind, 0},
	{"ks_next", 1, ks_next, 0},
	{"ks_current", 1, ks_current, 0},
	{"ks_get_cursor", 1, ks_get_cursor, 0},
	{"ks_set_cursor", 2, ks_set_cursor, 0},
	{"ks_at_cursor", 2, ks_at_cursor, 0},
	{"ks_lookup", 3, ks_lookup, 0},
	{"ks_lookup_by_name", 3, ks_lookup_by_name, 0},
	{"ks_search", 2, ks_search, 0},
};
// clang-format on

int load (ErlNifEnv * env, void ** priv_data, ERL_NIF_TERM load_info)
{
	int flags = ERL_NIF_RT_CREATE;

	KDB_RESOURCE_TYPE = enif_open_resource_type (env, NULL, "KDB", NULL, flags, NULL);
	KEY_RESOURCE_TYPE = enif_open_resource_type (env, NULL, "Key", NULL, flags, NULL);
	KEY_SET_RESOURCE_TYPE = enif_open_resource_type (env, NULL, "KeySet", NULL, flags, NULL);

	return 0;
}

_ERL_NIF_INIT (_ELEKTRA_NIF_MODULE_NAME, funcs, load, NULL, NULL, NULL)

#pragma GCC diagnostic pop
