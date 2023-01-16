defmodule Elektra.KdbTest do
  use ExUnit.Case
  doctest Elektra.Kdb

  test "open and close KDB" do
    {:ok, handle} = Elektra.Kdb.open()

    assert Elektra.Kdb.close(handle) == 0
  end
end
