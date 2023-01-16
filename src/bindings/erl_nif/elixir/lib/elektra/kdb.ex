defmodule Elektra.Kdb do
  use GenServer

  @opaque t() :: pid

  @type kdb() :: Elektra.Kdb.t()
  @type key() :: Elektra.Key.t()
  @type key_set() :: Elektra.KeySet.t()

  # Client API

  @doc """
  Open a connection to a KDB.

  ## Examples

      iex> {:ok, _handle} = Elektra.Kdb.open()
  """
  @spec open() :: GenServer.on_start()
  def open() do
    GenServer.start_link(__MODULE__, nil)
  end

  @doc """
  Close the connection to a KDB.
  """
  @spec close(kdb(), key()) :: integer
  def close(handle, error_key \\ nil) do
    GenServer.call(handle, {:close, error_key})
  end

  @doc """
  Append to `ks` the values stored in the key `parent_key` in `handle`.
  """
  @spec get(kdb(), key_set(), key()) :: integer
  def get(handle, ks, parent_key) do
    GenServer.call(handle, {:get, ks, parent_key})
  end

  @doc """
  Set the keys in `handle` according to `ks`. Only keys below `parent_key` are
  modified.
  """
  @spec set(kdb(), key_set(), key()) :: integer
  def set(handle, ks, parent_key) do
    GenServer.call(handle, {:set, ks, parent_key})
  end

  # Server API

  @impl true
  def init(nil) do
    contract_resource = :null
    error_key_resource = Elektra.System.key_new("user:/error/key")

    kdb_resource = Elektra.System.kdb_open(contract_resource, error_key_resource)

    Elektra.System.key_del(error_key_resource)

    {:ok, kdb_resource}
  end

  @impl true
  def handle_call({:close, error_key}, _from, kdb_resource) do
    error_key_resource = NifUtil.unwrap(error_key)

    rc = Elektra.System.kdb_close(kdb_resource, error_key_resource)
    {:stop, :normal, rc, nil}
  end

  @impl true
  def handle_call({:get, ks, parent_key}, _from, kdb_resource) do
    ks_resource = NifUtil.unwrap(ks)
    parent_key_resource = NifUtil.unwrap(parent_key)

    rc = Elektra.System.kdb_get(kdb_resource, ks_resource, parent_key_resource)
    {:reply, rc, kdb_resource}
  end

  @impl true
  def handle_call({:set, ks, parent_key}, _from, kdb_resource) do
    ks_resource = NifUtil.unwrap(ks)
    parent_key_resource = NifUtil.unwrap(parent_key)

    rc = Elektra.System.kdb_set(kdb_resource, ks_resource, parent_key_resource)
    {:reply, rc, kdb_resource}
  end

  @impl true
  def handle_call(:nif_resource, _from, kdb_resource) do
    {:reply, kdb_resource, kdb_resource}
  end

  @impl true
  def terminate(:normal, nil) do
    :ok
  end

  @impl true
  def terminate(_reason, kdb_resource) when not is_nil(kdb_resource) do
    Elektra.System.kdb_close(kdb_resource, :null)
  end
end
