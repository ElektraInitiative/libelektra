defmodule Elektra.System do
  @on_load :load_nifs

  def load_nifs do
    :erlang.load_nif(Path.join(:code.priv_dir(:elektra), "libnif_kdb"), 0)
  end

  def error_message(function) do
    function <> " not implemented." <>
      " This error means that the function defined in the Elixir module does" <>
      " not match the definition specified in the NIF."
  end

  # KDB

  def kdb_open(_contract, _parent_key) do
    raise error_message("kdb_open/2")
  end

  def kdb_close(_handle, _errorKey) do
    raise error_message("kdb_close/2")
  end

  def kdb_get(_handle, _returned, _parent_key) do
    raise error_message("kdb_get/3")
  end

  def kdb_set(_handle, _returned, _parent_key) do
    raise error_message("kdb_set/3")
  end

  # Key

  def key_new(_keyname) do
    raise error_message("key_new/1")
  end

  def key_copy(_dest, _source, _flags) do
    raise error_message("key_copy/3")
  end

  def key_clear(_key) do
    raise error_message("key_clear/1")
  end

  def key_del(_key) do
    raise error_message("key_del/1")
  end

  def key_inc_ref(_key) do
    raise error_message("key_inc_ref/1")
  end

  def key_dec_ref(_key) do
    raise error_message("key_dec_ref/1")
  end

  def key_get_ref(_key) do
    raise error_message("key_get_ref/1")
  end

  def key_copy_meta(_dest, _source, _meta_name) do
    raise error_message("key_copy_meta/3")
  end

  def key_copy_all_meta(_dest, _source) do
    raise error_message("key_copy_all_meta/2")
  end

  def key_get_meta(_key, _meta_name) do
    raise error_message("key_get_meta/2")
  end

  def key_set_meta(_key, _meta_name, _new_meta_string) do
    raise error_message("key_set_meta/3")
  end

  def key_meta(_key) do
    raise error_message("key_meta/1")
  end

  def key_cmp(_key1, _key2) do
    raise error_message("key_cmp/2")
  end

  def key_need_sync(_key) do
    raise error_message("key_need_sync/1")
  end

  def key_is_below(_key, _check) do
    raise error_message("key_is_below/2")
  end

  def key_is_below_or_same(_key, _check) do
    raise error_message("key_is_below_or_same/2")
  end

  def key_is_directly_below(_key, _check) do
    raise error_message("key_is_directly_below/2")
  end

  def key_is_binary(_key) do
    raise error_message("key_is_binary/1")
  end

  def key_is_string(_key) do
    raise error_message("key_is_string/1")
  end

  def key_name(_key) do
    raise error_message("key_name/1")
  end

  def key_get_name_size(_key) do
    raise error_message("key_get_name_size/1")
  end

  def key_set_name(_key, _new_name) do
    raise error_message("key_set_name/2")
  end

  def key_add_name(_key, _add_name) do
    raise error_message("key_add_name/2")
  end

  def key_unescaped_name(_key) do
    raise error_message("key_unescaped_name/1")
  end

  def key_get_unescaped_name_size(_key) do
    raise error_message("key_get_unescaped_name_size/1")
  end

  def key_base_name(_key) do
    raise error_message("key_base_name/1")
  end

  def key_get_base_name_size(_key) do
    raise error_message("key_get_base_name_size/1")
  end

  def key_set_base_name(_key, _base_name) do
    raise error_message("key_set_base_name/2")
  end

  def key_add_base_name(_key, _base_name) do
    raise error_message("key_add_base_name/2")
  end

  def key_get_namespace(_key) do
    raise error_message("key_get_namespace/1")
  end

  def key_set_namespace(_key, _ns) do
    raise error_message("key_set_namespace/2")
  end

  def key_value(_key) do
    raise error_message("key_value/1")
  end

  def key_get_value_size(_key) do
    raise error_message("key_get_value_size/1")
  end

  def key_string(_key) do
    raise error_message("key_string/1")
  end

  def key_set_string(_key, _new_string) do
    raise error_message("key_set_string/2")
  end

  def key_get_binary(_key) do
    raise error_message("key_get_binary/1")
  end

  def key_set_binary(_key, _new_binary) do
    raise error_message("key_set_binary/2")
  end

  def key_lock(_key, _flags) do
    raise error_message("key_lock/2")
  end

  def key_is_locked(_key) do
    raise error_message("key_is_locked/1")
  end

  def key_dup(_key, _flags) do
    raise error_message("key_dup/2")
  end

  # KeySet

  def ks_new(_size) do
    raise error_message("ks_new/1")
  end

  def ks_dup(_ks) do
    raise error_message("ks_dup/1")
  end

  def ks_copy(_dest, _source) do
    raise error_message("ks_copy/2")
  end

  def ks_inc_ref(_ks) do
    raise error_message("ks_inc_ref/1")
  end

  def ks_dec_ref(_ks) do
    raise error_message("ks_dec_ref/1")
  end

  def ks_get_ref(_ks) do
    raise error_message("ks_get_ref/1")
  end

  def ks_clear(_ks) do
    raise error_message("ks_clear/1")
  end

  def ks_del(_ks) do
    raise error_message("ks_del/1")
  end

  def ks_get_size(_ks) do
    raise error_message("ks_get_size/1")
  end

  def ks_append_key(_ks, _key_to_append) do
    raise error_message("ks_append_key/2")
  end

  def ks_append(_ks, _ks_to_append) do
    raise error_message("ks_append/2")
  end

  def ks_cut(_ks, _key_cutpoint) do
    raise error_message("ks_cut/2")
  end

  def ks_pop(_ks) do
    raise error_message("ks_pop/1")
  end

  def ks_rewind(_ks) do
    raise error_message("ks_rewind/1")
  end

  def ks_next(_ks) do
    raise error_message("ks_next/1")
  end

  def ks_current(_ks) do
    raise error_message("ks_current/1")
  end

  def ks_get_cursor(_ks) do
    raise error_message("ks_get_cursor/1")
  end

  def ks_set_cursor(_ks, _cursor) do
    raise error_message("ks_set_cursor/2")
  end

  def ks_at_cursor(_ks, _cursor) do
    raise error_message("ks_at_cursor/2")
  end

  def ks_lookup(_ks, _key, _options) do
    raise error_message("ks_lookup/3")
  end

  def ks_lookup_by_name(_ks, _name, _options) do
    raise error_message("ks_lookup_by_name/3")
  end

  def ks_search(_ks, _key) do
    raise error_message("ks_search/2")
  end
end
