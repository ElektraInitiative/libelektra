# Elixir bindings

The functions of the underlying C API are exposed by the modules in `Elektra.System`

* `Elektra.System.Kdb`
* `Elektra.System.Key`
* `Elektra.System.KeySet`

## Example

The analogue of the [`helloElektra.c`](../../../../examples/helloElektra.c) example is given by the following snippet

```elixir
defmodule Main do
  use Elektra

  def main do
    config = Elektra.System.KeySet.new() 
    root = Elektra.System.Key.new("user:/test")

    IO.puts("Open key database")
    handle = Elektra.System.Kdb.open()

    IO.puts("Retrieve key set")
    Elektra.System.Kdb.get(handle, config, root)

    IO.puts("Number of key-value pairs: #{Elektra.System.KeySet.get_size(config)}")

    key = Kdb.Key.new("user:/test/hello", "elektra")
    IO.puts("Add key #{Elektra.System.Key.base_name(key)}")
    Elektra.System.KeySet.append_key(config, key)
    IO.puts("Number of key-value pairs: #{Elektra.System.KeySet.get_size(config)}")
    IO.puts("#{Elektra.System.Key.base_name(key)}, #{Elektra.System.Key.string(key)}")

    # If you want to store the key database on disk, then please uncomment the following two lines
    # IO.puts("Write key set to disk")
    # :ok = Elektra.System.Kdb.set(handle, config, root)

    IO.puts("Delete key-value pairs inside memory")
    Elektra.System.KeySet.del(config)
    IO.puts("Close key database")
    Elektra.System.Kdb.close(handle)
  end
end

Main.main()
```
