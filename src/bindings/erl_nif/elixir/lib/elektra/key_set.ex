defmodule Elektra.KeySet do
  use GenServer

  @opaque t() :: pid

  @type kdb() :: Elektra.Kdb.t()
  @type key() :: Elektra.Key.t()
  @type key_set() :: Elektra.KeySet.t()

  # Client API

  # Server API

  @impl true
  def handle_call(:nif_resource, _from, ks_resource) do
    {:reply, ks_resource, ks_resource}
  end
end
