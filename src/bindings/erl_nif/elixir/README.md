# Elixir bindings

The functions of the underlying C API are exposed by the modules in `Elektra.System`

- `Elektra.System`

However, see [`src/bindings/erl_nif/README.md`](../README.md) for an overview of the differences to the C API and the limitations.

Each of the Elektra classes is also available as a Elixir modules which implement a `GenServer` behavior[^1]:

- `Elektra.Kdb`
- `Elektra.Key`
- `Elektra.KeySet`

## Conveniences

### Creating keys using maps

```elixir
Elektra.Key.new(%{name: "user:/test"})
Elektra.Key.new(%{name: "user:/test", value: "value"})
Elektra.Key.new(
  %{
    name: "user:/test",
    value: "value",
    meta: [
      {"metaname1", "metavalue1"},
      {"metaname2", "metavalue2"},
    ],
  }
)
```

### Converting keys to maps

```elixir
{name: name, value: value} = Elektra.Key.to_map(key)
```

### Generate a stream of keys from a key set

```elixir
key_stream = Elektra.KeySet.stream(ks)
```

### Iterating over key sets

Using the `Elektra.KeySet.stream` function it is straightforward to iterate over the keys of a key set

```elixir
ks
|> Elektra.KeySet.stream
|> Stream.map(&Elektra.Key.to_map/1)
|> Enum.each(fn %{name: name, value: value} ->
  IO.puts("#{name}, #{value}")
end)
```

## Examples

### Hello Elektra with `Elektra.System`

The analogue of the [`helloElektra.c`](../../../../examples/helloElektra.c) example is given by the following snippet

```elixir
defmodule Main do
  use Elektra

  def main do
    error_key = Elektra.System.key_new("user:/error/key")

    config = Elektra.System.ks_new(0)
    root = Elektra.System.key_new("user:/test")

    IO.puts("Open key database")
    handle = Elektra.System.kdb_open(:null, error_key)

    IO.puts("Retrieve key set")
    Elektra.System.kdb_get(handle, config, root)

    IO.puts("Number of key-value pairs: #{Elektra.System.ks_get_size(config)}")

    key = Elektra.System.key_new("user:/test/hello")
    Elektra.System.key_set_string(key, "elektra")
    IO.puts("Add key #{Elektra.System.key_base_name(key)}")
    Elektra.System.ks_append_key(config, key)
    IO.puts("Number of key-value pairs: #{Elektra.System.ks_get_size(config)}")
    IO.puts("#{Elektra.System.key_base_name(key)}, #{Elektra.System.key_string(key)}")

    # If you want to store the key database on disk, then please uncomment the following two lines
    # IO.puts("Write key set to disk")
    # Elektra.System.kdb_set(handle, config, root)

    IO.puts("Delete key-value pairs inside memory")
    Elektra.System.ks_del(config)
    IO.puts("Close key database")
    Elektra.System.kdb_close(handle, error_key)

    Elektra.System.key_del(error_key)
  end
end

Main.main()
```

### Hello Elektra with `Elektra`

```elixir
defmodule Main do
  use Elektra

  def main do
    {:ok, config} = Elektra.KeySet.new()
    {:ok, root} = Elektra.Key.new(%{name: "user:/test"})

    IO.puts("Open key database")
    {:ok, handle} = Elektra.Kdb.open()

    IO.puts("Retrieve key set")
    Elektra.Kdb.get(handle, config, root)

    IO.puts("Number of key-value pairs: #{Elektra.KeySet.get_size(config)}")

    {:ok, key} = Elektra.Key.new(%{name: "user:/test/hello", value: "elektra"})
    IO.puts("Add key #{Elektra.Key.base_name(key)}")
    Elektra.KeySet.append_key(config, key)
    IO.puts("Number of key-value pairs: #{Elektra.KeySet.get_size(config)}")
    IO.puts("#{Elektra.Key.base_name(key)}, #{Elektra.Key.string(key)}")

    # If you want to store the key database on disk, then please uncomment the following two lines
    # IO.puts("Write key set to disk")
    # Elektra.Kdb.set(handle, config, root)

    IO.puts("Delete key-value pairs inside memory")
    Elektra.KeySet.del(config)
    IO.puts("Close key database")
    Elektra.Kdb.close(handle)
  end
end

Main.main()
```

## Installation

To have access to the Elixir module you need to compile Elektra with `elixir` added to the CMake option `BINDINGS`.
For instance, the configuration inside a build directory might be (assuming the build directory is itself in the root directory of Elektra)

```sh
cmake -DBINDINGS=elixir ..
```

The Elixir module is then available in `libelektra/build/src/bindings/erl_nif/elixir`.
You can copy the module directory to any convenient location.

When using Mix the dependency can be specified as follows in `mix.exs`

```elixir
defp deps do
  [
    {:elektra, path: "/path/to/the/elixir/module"}
  ]
end
```

Note that the Elixir module has Elektra as a dependency and the Elektra libraries need to be installed.

[^1]:
    We adopt the American spelling due to technical limitations of the spell checking script.
    The correct spelling in the context of Erlang/OTP is the British spelling.
