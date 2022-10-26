#include <kdb.h>

#include "erl_nif.h"

static ERL_NIF_TERM
nif_ks_new(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  KeySet * ks = ksNew (0, KS_END);
  return enif_make_resource(env, ks);
}

static ErlNifFunc nif_funcs[] = {
  {"nif_ks_new", 0, nif_ks_new},
};

ERL_NIF_INIT(Elixir.Elektra.Kdb, nif_funcs, NULL, NULL, NULL, NULL);

