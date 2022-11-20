defmodule Elektra.KeySet do
  use GenServer

  @opaque t() :: pid

  @type kdb() :: Elektra.Kdb.t()
  @type key() :: Elektra.Key.t()
  @type key_set() :: Elektra.KeySet.t()

  # Client API

  @doc """
  Create a new key set with pre-allocated memory of size `size`.
  """
  @spec new(non_neg_integer()) :: key_set()
  def new(size \\ 0) do
    GenServer.start_link(__MODULE__, size)
  end

  # Server API

  @impl true
  def init(size) do
    ks_resource = Elektra.System.ks_new(size)
    {:ok, ks_resource}
  end

  @impl true
  def handle_call(:nif_resource, _from, ks_resource) do
    {:reply, ks_resource, ks_resource}
  end
end
