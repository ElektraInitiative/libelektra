defmodule Elektra.KeySet do
  use GenServer

  @opaque t() :: pid

  @type kdb() :: Elektra.Kdb.t()
  @type key() :: Elektra.Key.t()
  @type key_set() :: Elektra.KeySet.t()

  # Client API

  @doc """
  Create a new key set with pre-allocated memory of size `size`.

  ## Examples

      iex> {:ok, _ks} = Elektra.KeySet.new(42)
  """
  @spec new(non_neg_integer()) :: key_set()
  def new(size \\ 0) do
    GenServer.start_link(__MODULE__, size)
  end

  @doc """
  Create a key set from the NIF resource `ks_resource`.
  """
  @spec from_resource(reference()) :: key_set()
  def from_resource(ks_resource) do
    GenServer.start_link(__MODULE__, ks_resource)
  end

  @doc """
  Create a stream of keys from `ks`.
  """
  @spec stream(key_set()) :: Enumerable.t()
  def stream(ks) do
    GenServer.call(ks, :stream)
  end

  @doc """
  Add the key `key` to the key set `ks`.
  """
  @spec append_key(key_set(), key()) :: integer()
  def append_key(ks, key) do
    GenServer.call(ks, {:append_key, key})
  end

  @doc """
  Get the size of the key set `ks`.
  """
  @spec get_size(key_set()) :: integer()
  def get_size(ks) do
    GenServer.call(ks, :get_size)
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
  def init(size) when is_integer(size) do
    ks_resource = Elektra.System.ks_new(size)
    {:ok, ks_resource}
  end

  @impl true
  def init(ks_resource) when is_reference(ks_resource) do
    {:ok, ks_resource}
  end

  @impl true
  def handle_call(:stream, _from, ks_resource) do
    stream =
      Stream.resource(
        fn ->
          {
            ks_resource,
            0,
            Elektra.System.ks_get_size(ks_resource)
          }
        end,
        fn {ks_resource, i, len} ->
          if i < len do
            key_resource = Elektra.System.ks_at_cursor(ks_resource, i)
            {:ok, key} = Elektra.Key.from_resource(key_resource)
            {[key], {ks_resource, i + 1, len}}
          else
            {:halt, ks_resource}
          end
        end,
        fn _ks_resource -> nil end
      )

    {:reply, stream, ks_resource}
  end

  @impl true
  def handle_call({:append_key, key}, _from, ks_resource) do
    key_resource = NifUtil.unwrap(key)
    rc = Elektra.System.ks_append_key(ks_resource, key_resource)
    {:reply, rc, ks_resource}
  end

  @impl true
  def handle_call(:get_size, _from, ks_resource) do
    rc = Elektra.System.ks_get_size(ks_resource)
    {:reply, rc, ks_resource}
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
