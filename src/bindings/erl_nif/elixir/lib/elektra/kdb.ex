defmodule Elektra.Kdb do
  use GenServer

  @opaque t() :: pid

  @type kdb() :: Elektra.Kdb.t()
  @type key() :: Elektra.Key.t()
  @type key_set() :: Elektra.KeySet.t()

  # Client API

  @doc """
  Open a connection to a KDB.
  """
  @spec open(key_set(), key()) :: GenServer.on_start()
  def open(contract, parent_key) do
    GenServer.start_link(__MODULE__, {contract, parent_key})
  end

  @doc """
  Close the connection to a KDB.
  """
  @spec close(kdb(), key()) :: integer
  def close(handle, error_key \\ nil) do
    GenServer.call(handle, {:close, error_key})
  end

  @doc """
  Append to `ks` the values stored in at the key `parent_key` in `handle`.
  """
  @spec get(kdb(), key_set(), key()) :: integer
  def get(handle, ks, parent_key) do
    GenServer.call(handle, {:get, ks, parent_key})
  end

  @doc"""
  Set the keys in `handle` according to `ks`. Only keys below `parent_key` are
  modified.
  """
  @spec set(kdb(), key_set(), key()) :: integer
  def set(handle, ks, parent_key) do
    GenServer.call(handle, {:set, ks, parent_key})
  end

  # Server API

  @impl true
  def init({contract, parent_key}) do
    contract = NifUtil.unwrap(contract)
    parent_key = NifUtil.unwrap(parent_key)

    kdb = Elektra.System.kdb_open(contract, parent_key)
    {:ok, kdb}
  end

  @impl true
  def handle_call({:close, error_key}, _from, kdb) do
    error_key = NifUtil.unwrap(error_key)

    rc = Elektra.System.kdb_close(kdb, error_key)
    {:stop, :normal, rc, kdb}
  end

  @impl true
  def handle_call({:get, {ks, parent_key}}, _from, kdb) do
    ks = NifUtil.unwrap(ks)
    parent_key = NifUtil.unwrap(parent_key)

    rc = Elektra.System.kdb_get(kdb, ks, parent_key)
    {:reply, rc, kdb}
  end

  @impl true
  def handle_call({:set, {ks, parent_key}}, _from, kdb) do
    ks = NifUtil.unwrap(ks)
    parent_key = NifUtil.unwrap(parent_key)

    rc = Elektra.System.kdb_set(kdb, ks, parent_key)
    {:reply, rc, kdb}
  end

  @impl true
  def handle_call(:nif_resource, _from, kdb) do
    {:reply, kdb, kdb}
  end
end
