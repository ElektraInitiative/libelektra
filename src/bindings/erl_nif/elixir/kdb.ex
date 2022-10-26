defmodule Elektra.Kdb do
  @on_load :load_nifs

  def load_nifs do
    :erlang.load_nif('../build/libnif_kdb', 0)
  end

  def nif_ks_new() do
    raise "NIF nif_ks_new/0 not implemented"
  end
end
