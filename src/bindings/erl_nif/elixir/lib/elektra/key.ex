defmodule Elektra.Key do
  use GenServer

  @enforce_keys [:name]
  defstruct [
    :name,
    :value,
    {:meta, []}
  ]

  @opaque t() :: pid

  @type kdb() :: Elektra.Kdb.t()
  @type key() :: Elektra.Key.t()
  @type key_set() :: Elektra.KeySet.t()

  @type meta_list() :: list({String.t(), String.t()})

  # Client API

  @doc """
  Create a new key from `map`.

  ## Examples

      iex> {:ok, key} = Elektra.Key.new(%{name: "user:/test"})

      iex> key_map = %{name: "user:/test", value: "thevalue"}
      iex> {:ok, key} = Elektra.Key.new(key_map)

      iex> key_map = %{
      ...>   name: "user:/test",
      ...>   value: "thevalue",
      ...>   meta: [
      ...>     {"metaname1", "metavalue1"},
      ...>     {"metaname2", "metavalue2"},
      ...>   ],
      ...> }
      iex> {:ok, key} = Elektra.Key.new(key_map)
  """
  @spec new(%{name: String.t(), value: String.t(), meta: meta_list()}) :: key()
  def new(map) when is_map(map) do
    GenServer.start_link(__MODULE__, struct(__MODULE__, map))
  end

  @doc """
  Get the name of the `key`.

  ## Examples

      iex> {:ok, key} = Elektra.Key.new(%{name: "user:/test"})
      iex> Elektra.Key.name(key)
      "user:/test"
  """
  @spec name(key()) :: String.t()
  def name(key) do
    GenServer.call(key, :name)
  end

  # Server API

  @impl true
  def init(%__MODULE__{name: name, value: value, meta: meta}) do
    key_resource = Elektra.System.key_new(name)

    if value != nil do
      Elektra.System.key_set_string(key_resource, value)
    end

    for {meta_name, meta_value} <- meta do
      Elektra.System.key_set_meta(key_resource, meta_name, meta_value)
    end

    {:ok, key_resource}
  end

  @impl true
  def handle_call(:name, _from, key_resource) do
    name = Elektra.System.key_name(key_resource)
    {:reply, name, key_resource}
  end

  @impl true
  def handle_call(:del, _from, key_resource) do
    rc = Elektra.System.key_del(key_resource)
    {:stop, :normal, rc, nil}
  end

  @impl true
  def handle_call(:nif_resource, _from, key_resource) do
    {:reply, key_resource, key_resource}
  end

  @impl true
  def terminate(:normal, nil) do
    :ok
  end

  @impl true
  def terminate(_reason, key_resource) when not is_nil(key_resource) do
    Elektra.System.key_del(key_resource)
  end
end
