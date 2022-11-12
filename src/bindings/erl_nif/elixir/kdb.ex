defmodule Elektra.System do
  @on_load :load_nifs

  def load_nifs do
    :erlang.load_nif('../build/libnif_kdb', 0)
  end

  # KDB

  def nif_kdb_open(_contract, _parent_key) do
    raise "NIF nif_kdb_open/2 not implemented"
  end

  def nif_kdb_close(_handle, _errorKey) do
    raise "NIF nif_kdb_close/2 not implemented"
  end

  def nif_kdb_get(_handle, _returned, _parent_key) do
    raise "NIF nif_kdb_get/3 not implemented"
  end

  def nif_kdb_set(_handle, _returned, _parent_key) do
    raise "NIF nif_kdb_set/3 not implemented"
  end

  # Key

  def nif_key_new(_name) do
    raise "NIF nif_key_new/1 not implemented"
  end

  def nif_key_set_name(_key, _name) do
    raise "NIF nif_key_set_name/2 not implemented"
  end

  def nif_key_add_name(_key, _add_name) do
    raise "NIF nif_key_add_name/2 not implemented"
  end

  def nif_key_name(_key) do
    raise "NIF nif_key_name/1 not implemented"
  end

  def nif_key_string(_key) do
    raise "NIF nif_key_string/1 not implemented"
  end

  # KeySet

  def nif_ks_new() do
    raise "NIF nif_ks_new/0 not implemented"
  end

  def nif_ks_append_key(_ks, _to_append) do
    raise "NIF nif_ks_append_key/2 not implemented"
  end

  def nif_ks_lookup_by_name(_ks, _name, _options) do
    raise "NIF nif_ks_lookup_by_name/3 not implemented"
  end
end
