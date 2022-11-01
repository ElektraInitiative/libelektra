defmodule Elektra.System do
  @on_load :load_nifs

  def load_nifs do
    :erlang.load_nif('../build/libnif_kdb', 0)
  end

  def nif_ks_new() do
    raise "NIF nif_ks_new/0 not implemented"
  end

  def nif_ks_append_key(_ks, _to_append) do
    raise "NIF nif_ks_append_key/2 not implemented"
  end

  def nif_kdb_open(_contract, _parent_key) do
    raise "NIF nif_kdb_open/2 not implemented"
  end

  def nif_kdb_open(_parent_key) do
    raise "NIF nif_kdb_open/1 not implemented"
  end

  def nif_kdb_get(_handle, _returned, _parent_key) do
    raise "NIF nif_kdb_get/3 not implemented"
  end

  def nif_kdb_set(_handle, _returned, _parent_key) do
    raise "NIF nif_kdb_set/3 not implemented"
  end

  def nif_key_new(_keyname) do
    raise "NIF nif_key_new/1 not implemented"
  end

  def nif_key_new(_keyname, _keyvalue) do
    raise "NIF nif_key_new/2 not implemented"
  end
end
