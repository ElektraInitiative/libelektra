defmodule Elektra.Nif do
  @on_load :load_nifs

  def load_nifs do
    :erlang.load_nif('../build/libnif_kdb', 0)
  end

  def nif_ks_new() do
    raise "NIF nif_ks_new/0 not implemented"
  end

  def nif_kdb_open(_contract, _parent_key) do
    raise "NIF nif_kdb_open/2 not implemented"
  end

  def nif_key_new(_keyname) do
    raise "NIF nif_key_new/1 not implemented"
  end
end
