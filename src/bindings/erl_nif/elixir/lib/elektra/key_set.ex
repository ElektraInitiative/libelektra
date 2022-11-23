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

  @doc """
  Delete the key set `ks` from memory.
  """
  @spec del(key_set()) :: integer()
  def del(ks) do
    GenServer.call(ks, :del)
  end

  # Server API

  @impl true
  def init(size) do
    ks_resource = Elektra.System.ks_new(size)
    {:ok, ks_resource}
  end

  @impl true
  def handle_call(:del, _from, ks_resource) do
    rc = Elektra.System.ks_del(ks_resource)
    {:reply, rc, nil}
  end

  @impl true
  def handle_call(:nif_resource, _from, ks_resource) do
    {:reply, ks_resource, ks_resource}
  end

  @impl true
  def terminate(:normal, nil) do
    :ok
  end

  @impl true
  def terminate(_reason, ks_resource) when not is_nil(ks_resource) do
    Elektra.System.ks_del(ks_resource)
  end
end
