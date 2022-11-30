defmodule Elektra do
  defmacro __using__(_opts) do
    quote do
      import Elektra.System

      import Elektra.Kdb
      import Elektra.KeySet
      import Elektra.Key
    end
  end
end
