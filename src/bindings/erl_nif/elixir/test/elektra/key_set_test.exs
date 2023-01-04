defmodule Elektra.KeySetTest do
  use ExUnit.Case
  doctest Elektra.KeySet

  test "creating and deleting" do
    {:ok, ks} = Elektra.KeySet.new()

    assert Elektra.KeySet.del(ks) == 0
  end

  test "appending keys" do
    {:ok, ks} = Elektra.KeySet.new()

    assert Elektra.KeySet.get_size(ks) == 0

    {:ok, key} = Elektra.Key.new(%{name: "user:/test/1", value: "a"})
    Elektra.KeySet.append_key(ks, key)

    assert Elektra.KeySet.get_size(ks) == 1
  end

  test "stream" do
    {:ok, ks} = Elektra.KeySet.new()

    {:ok, key1} = Elektra.Key.new(%{name: "user:/test/1", value: "a"})
    Elektra.KeySet.append_key(ks, key1)

    {:ok, key2} = Elektra.Key.new(%{name: "user:/test/2", value: "b"})
    Elektra.KeySet.append_key(ks, key2)

    from_stream =
      Elektra.KeySet.stream(ks)
      |> Enum.to_list()
      |> Enum.map(&Elektra.Key.to_map/1)
      |> Enum.map(fn %{name: name, value: value} -> {name, value} end)
      |> Enum.sort_by(&elem(&1, 0))

    expected = [
      {"user:/test/1", "a"},
      {"user:/test/2", "b"}
    ]

    assert from_stream == expected
  end
end
