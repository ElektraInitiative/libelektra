#include <stdio.h>
#include <kdb.h>

#include "erl_nif.h"

ErlNifResourceType * KDB_RESOURCE_TYPE;
ErlNifResourceType * KEY_RESOURCE_TYPE;
ErlNifResourceType * KEY_SET_RESOURCE_TYPE;

// This destructor is called when Erlang no longer holds a reference to the KDB object.
void
kdb_resource_destructor(ErlNifEnv * env, void * res) {
  KDB * handle = (KDB *) res;
  kdbClose (handle, NULL);
}

void
key_resource_destructor(ErlNifEnv * env, void * res) {
  KeySet * ks = (KeySet *) res;
  ksDel (ks);
}

void
key_set_resource_destructor(ErlNifEnv * env, void * res) {
  // TODO: Create destructor
  return;
}

/**************************************
 *
 * KDB methods
 *
 **************************************/

// KDB * kdbOpen (const KeySet * contract, Key *parentKey);
static ERL_NIF_TERM
nif_kdb_open(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  KeySet * contract;
  Key * parentKey;

  if (
      !enif_get_resource(env, argv[0], KEY_SET_RESOURCE_TYPE, (void *) &contract) ||
      !enif_get_resource(env, argv[1], KEY_RESOURCE_TYPE, (void *) &parentKey)
      ) {
    return enif_make_badarg(env);
  }

  KDB * kdb_resource = enif_alloc_resource(KDB_RESOURCE_TYPE, sizeof (KDB *));

  KDB * kdb = kdbOpen(contract, parentKey);
  kdb_resource = kdb;

  ERL_NIF_TERM term = enif_make_resource(env, kdb_resource);
  enif_release_resource(kdb_resource);

  return term;
}

// int kdbClose (KDB *handle, Key *errorKey);
// 
// int kdbGet (KDB *handle, KeySet *returned,
// 	Key *parentKey);
// int kdbSet (KDB *handle, KeySet *returned,
// 	Key *parentKey);


static ERL_NIF_TERM
nif_ks_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {

  KeySet * ks_resource = enif_alloc_resource(KEY_SET_RESOURCE_TYPE, sizeof (KeySet *));

  KeySet * ks = ksNew (0, KS_END);
  ks_resource = ks;

  ERL_NIF_TERM term = enif_make_resource(env, ks_resource);
  enif_release_resource(ks_resource);

  return term;
}

static ERL_NIF_TERM
nif_key_new(ErlNifEnv * env, int argc, const ERL_NIF_TERM argv[]) {
  char keyname[KEY_SIZE];

  if (
        enif_get_string(env, argv[0], keyname, KEY_SIZE, ERL_NIF_LATIN1)
        <= 0
     ) {
    return enif_make_badarg(env);
  }

  Key * key_resource = enif_alloc_resource(KEY_RESOURCE_TYPE, sizeof (Key *));

  Key * key = keyNew (keyname, KEY_END);
  key_resource = key;

  ERL_NIF_TERM term = enif_make_resource(env, key_resource);
  enif_release_resource(key_resource);

  return term;
}

static ErlNifFunc nif_funcs[] = {
  {"nif_ks_new", 0, nif_ks_new},
  {"nif_kdb_open", 2, nif_kdb_open},
  {"nif_key_new", 1, nif_key_new},
};

// Load
int
load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;

  //KDB_RESOURCE_TYPE = enif_open_resource_type(env, NULL, "KDB", kdb_resource_destructor, flags, NULL);
  //KEY_RESOURCE_TYPE = enif_open_resource_type(env, NULL, "Key", key_resource_destructor, flags, NULL);
  KEY_SET_RESOURCE_TYPE = enif_open_resource_type(env, NULL, "KeySet", key_set_resource_destructor, flags, NULL);

  printf("Loading...\n");

  return 0;
}

ERL_NIF_INIT(Elixir.Elektra.Nif, nif_funcs, load, NULL, NULL, NULL);

