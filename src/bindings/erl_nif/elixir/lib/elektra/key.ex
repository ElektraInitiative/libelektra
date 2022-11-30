defmodule Elektra.Key do
  use GenServer

  @enforce_keys [:name]
  defstruct [
    :name,
    :value,
    {:meta, []}
  ]

  @default_opts [
    binary: false
  ]

  @type opts() :: Keyword.t()

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
  def new(map = %{name: _name}) do
    GenServer.start_link(__MODULE__, struct(__MODULE__, map))
  end

  @doc """
  Create a key from `ref`.
  """
  @spec from_resource(reference()) :: key()
  def from_resource(key_resource) when is_reference(key_resource) do
    GenServer.start_link(__MODULE__, key_resource)
  end

  @doc """
  Get the name of the key `key`.

  ## Examples

      iex> {:ok, key} = Elektra.Key.new(%{name: "user:/test"})
      iex> Elektra.Key.name(key)
      "user:/test"
  """
  @spec name(key()) :: String.t()
  def name(key) do
    GenServer.call(key, :name)
  end

  @doc """
  Get the base name of the key `key`.
  """
  @spec base_name(key()) :: String.t()
  def base_name(key) do
    GenServer.call(key, :base_name)
  end

  @doc """
  Get the string value of the key `key`.
  """
  @spec string(key()) :: String.t()
  def string(key) do
    GenServer.call(key, :string)
  end

  @doc """
  Get the value of the key `key`.
  """
  @spec value(key()) :: binary()
  def value(key) do
    GenServer.call(key, :value)
  end

  @doc """
  Get a key set of the meta keys of the key `key`.
  """
  @spec meta(key()) :: key_set()
  def meta(key) do
    GenServer.call(key, :meta)
  end

  @doc """
  Get a map corresponding to the name and value of the key `key`.
  """
  @spec to_map(key(), opts()) :: %{name: String.t(), value: binary(), meta: meta_list()}
  def to_map(key, opts \\ []) do
    opts = Keyword.merge(@default_opts, opts)
    name = Elektra.Key.name(key)

    value =
      if opts[:binary] do
        Elektra.Key.value(key)
      else
        Elektra.Key.string(key)
      end

    meta =
      Elektra.Key.meta(key)
      |> Elektra.KeySet.stream()
      |> Stream.map(&Elektra.Key.to_map/1)
      |> Enum.map(fn %{name: name, value: value} -> {name, value} end)

    %{name: name, value: value, meta: meta}
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
  def init(ref) when is_reference(ref) do
    {:ok, ref}
  end

  @impl true
  def handle_call(:name, _from, key_resource) do
    name = Elektra.System.key_name(key_resource)
    {:reply, name, key_resource}
  end

  @impl true
  def handle_call(:base_name, _from, key_resource) do
    base_name = Elektra.System.key_base_name(key_resource)
    {:reply, base_name, key_resource}
  end

  @impl true
  def handle_call(:string, _from, key_resource) do
    string = Elektra.System.key_string(key_resource)
    {:reply, string, key_resource}
  end

  @impl true
  def handle_call(:value, _from, key_resource) do
    value = Elektra.System.key_value(key_resource)
    {:reply, value, key_resource}
  end

  @impl true
  def handle_call(:meta, _from, key_resource) do
    ks_resource = Elektra.System.key_meta(key_resource)
    {:ok, ks} = Elektra.KeySet.from_resource(ks_resource)
    {:reply, ks, key_resource}
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
