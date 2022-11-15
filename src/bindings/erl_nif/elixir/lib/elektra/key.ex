defmodule Elektra.Key do
  use GenServer

  @opaque t() :: pid

  @type kdb() :: Elektra.Kdb.t()
  @type key() :: Elektra.Key.t()
  @type key_set() :: Elektra.KeySet.t()

  # Client API

  # Server API

  @impl true
  def handle_call(:nif_resource, _from, key_resource) do
    {:reply, key_resource, key_resource}
  end
end
