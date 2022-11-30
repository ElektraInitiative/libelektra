defmodule Elektra.KeyTest do
  use ExUnit.Case
  doctest Elektra.Key

  test "to_map is the inverse of new" do
    key_map = %{
      name: "user:/test/thetestkey",
      value: "theteststring",
      meta: [
        {"meta1", "metaval1"},
        {"meta2", "metaval2"}
      ]
    }

    {:ok, key} = Elektra.Key.new(key_map)

    assert Map.equal?(
             Elektra.Key.to_map(key),
             key_map
           )
  end
end
