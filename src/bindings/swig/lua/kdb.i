%module kdb

%include "../common.i"

%wrapper {
  /* adds a variable/property to a class */
  void add_class_variable(lua_State *L, const char *classname,
    const char *name, lua_CFunction getFn,lua_CFunction setFn)
  {
    SWIG_Lua_get_class_metatable(L, classname);
#if SWIG_VERSION > 0x030000
    SWIG_Lua_add_variable(L, name, getFn, setFn);
#else
    SWIG_Lua_add_class_variable(L, name, getFn, setFn);
#endif
    lua_pop(L, 1);
  }

  /* adds a method to a class */
  void add_class_method(lua_State *L, const char *classname,
    const char *method, lua_CFunction fn)
  {
    SWIG_Lua_get_class_metatable(L, classname);

    if (lua_istable(L,-1))
    {
      SWIG_Lua_get_table(L, ".fn");
      SWIG_Lua_add_function(L, method, fn);
      lua_pop(L, 1);
    }
    lua_pop(L, 1);
  }
}


/* handle exceptions */
%{
  #define KDB_CATCH_EX(namespace, exception) \
    catch(const namespace::exception &e) \
    { \
      lua_pushfstring(L, "%s:%s", #namespace, e.what()); \
      SWIG_fail; \
    }
%}


/*
 * key.hpp
 */
// exception handling for kdb::Key
%exception {
  KDB_CATCH(KEY_EXCEPTIONS)
}

// operator overloading sucks
%ignore kdb::Key::operator bool;

// properties
// lua supports properties and also wraps in the exception code - yeah!
%attributestring(kdb::Key, std::string, name,     getName, setName);
%attributestring(kdb::Key, std::string, basename, getBaseName, setBaseName);
%attributestring(kdb::Key, std::string, dirname,  getDirName);
%attributestring(kdb::Key, std::string, fullname, getFullName);

%ignore kdb::Key::getName;
%ignore kdb::Key::setName;
%ignore kdb::Key::getBaseName;
%ignore kdb::Key::setBaseName;
%ignore kdb::Key::getDirName;
%ignore kdb::Key::getFullName;

// we handle binary just like strings with additional length param
%typemap(out) std::string kdb::Key::getBinary {
  lua_pushlstring(L, $1.data(), $1.size());
  SWIG_arg++;
}

%typemap(in, checkfn="lua_isstring") (const void *newBinary, size_t dataSize) {
  $2 = lua_rawlen(L, $input);
  $1 = const_cast<char *>(lua_tostring(L, $input));
}

%typemap(out) void *kdb::Key::getValue {
  ssize_t size = arg1->getBinarySize();
  lua_pushlstring(L, (const char *)$1, (size > 0) ? size : 0);
  SWIG_arg++;
}

%extend kdb::Key {
  Key(const char *keyName) {
    return new kdb::Key(keyName, KEY_END);
  }

  bool isNull() {
    return !bool(*self);
  }

  const char *__tostring() {
    return self->getName().c_str();
  }
};

%{
  /*
   * returns string or binary value depending on the type of the key
   */
  int _my_Key_getValue(lua_State* L)
  {
    lua_pushvalue(L, 1); /* push copy of self */
    lua_getfield(L, -1, "isBinary");
    lua_insert(L, -2);   /* insert function in the middle */
    lua_call(L, 1, 1);
    bool binary = lua_toboolean(L, -1);
    lua_pop(L, 1);      /* remove result */
    lua_getfield(L, -1, (binary) ? "binary" : "string");
    return 1;
  }
%}

%init %{
  // other methods/properties aliases
  add_class_method(L,   "Key", "get",    _my_Key_getValue);
  add_class_variable(L, "Key", "value",  _my_Key_getValue,     SWIG_Lua_set_immutable);
  add_class_variable(L, "Key", "string", _wrap_Key__getString, _wrap_Key__setString);
  add_class_variable(L, "Key", "binary", _wrap_Key__getBinary, _wrap_Key__setBinary);
%}

// add/override some other useful methods
%luacode %{
  local orig_call = kdb.Key
  kdb.Key = function(name, ...)
    local key = orig_call(name)

    if select("#", ...) > 0 then
      local t = { ... }
      local i, arg = next(t, nil)
      while i do
        if arg == kdb.KEY_END then
          break
        elseif arg == kdb.KEY_NAME then
          i, key.name = next(t, i)
        elseif arg == kdb.KEY_VALUE then
          if key:isBinary() then
            i, key.binary = next(t, i)
          else
            i, key.string = next(t, i)
          end
        elseif arg == kdb.KEY_OWNER then
          i, arg = next(t, i)
          key:setMeta("owner", arg)
        elseif arg == kdb.KEY_COMMENT then
          i, arg = next(t, i)
          key:setMeta("comment", arg)
        elseif arg == kdb.KEY_BINARY then
          key:setMeta("binary", "")
        elseif arg == kdb.KEY_UID then
          i, arg = next(t, i)
          key:setMeta("uid", tostring(arg))
        elseif arg == kdb.KEY_GID then
          i, arg = next(t, i)
          key:setMeta("gid", tostring(arg))
        elseif arg == kdb.KEY_MODE then
          i, arg = next(t, i)
          key:setMeta("mode", string.format("%o", arg))
        elseif arg == kdb.KEY_ATIME then
          i, arg = next(t, i)
          key:setMeta("atime", string.format("%d", arg))
        elseif arg == kdb.KEY_MTIME then
          i, arg = next(t, i)
          key:setMeta("mtime", string.format("%d", arg))
        elseif arg == kdb.KEY_CTIME then
          i, arg = next(t, i)
          key:setMeta("ctime", string.format("%d", arg))
        elseif arg == kdb.KEY_SIZE then
          -- nothing
        elseif arg == kdb.KEY_FUNC then
          error("Unsupported meta type")
        elseif arg == kdb.KEY_DIR then
          require("bit32");
          local meta = key:getMeta("mode")
          local mode = not meta:isNull() and tonumber(meta.value, 8) or 0
          key:setMeta("mode", string.format("%o", bit32.bor(mode, tonumber(111, 8))))
        elseif arg == kdb.KEY_META then
          local tmp
          i, tmp = next(t, i)
          i, arg = next(t, i)
          key:setMeta(tmp, arg)
        elseif arg == kdb.KEY_NULL then
        else
          if kdb.DEBUG > 0 then
            io.stderr:write("Unknown option in keyNew ", arg, "\n")
          end
        end
        i, arg = next(t, i)
      end
    end

    return key
  end

  local mt = getmetatable(kdb.Key())
  mt[".fn"]["__metaIter"] = function(self)
     return coroutine.wrap(
      function()
        self:_rewindMeta()
        local meta = self:_nextMeta()
        while not meta:isNull() do
          coroutine.yield(meta)
          meta = self:_nextMeta()
        end
      end
    )
  end

  mt[".fn"]["getMeta"] = function(self, ...)
    if select("#", ...) > 0 then
      local meta = self:_getMeta(...)
      return not meta:isNull() and meta or nil
    end
    return self:__metaIter()
  end
%}

%include "key.hpp"

// meta data
%template(_getMeta) kdb::Key::getMeta<const kdb::Key>;
%template(setMeta) kdb::Key::setMeta<std::string>;

// clear exception handler
%exception;


/*
 * keyset.hpp
 */
%ignore kdb::KeySet::size;

%rename("_%s") kdb::KeySet::rewind;
%rename("_%s") kdb::KeySet::next;
%rename("_%s") kdb::KeySet::current;

%rename("_%s") kdb::KeySet::lookup;
%rename("_lookup") kdb::KeySet::at;

%extend kdb::KeySet {
  KeySet(size_t alloc) {
    return new kdb::KeySet(alloc, KS_END);
  }

  size_t __len(void *) {
    return self->size();
  }
};

%{
#if 0
  /*
   * advanced cursor variant: use KeySetIterator as invariant
   * NOTE: light userdata misses a metadata table so no __gc
   * causing the iterator to never get deleted
   */
  int _my_KeySet_ipairs_it(lua_State* L)
  {
    /* see comment of function below why this has been disabled */
    KeySet::iterator *it;
    const Key *key;

    it = (KeySet::iterator*)lua_touserdata(L, -2);
    if (*it == it->getKeySet().end())
      return 0;

    key = new kdb::Key(it->get());
    (*it)++;
    lua_Number i = lua_tonumber(L, -1);
    lua_pop(L, 2);

    lua_pushnumber(L, i + 1);
    SWIG_NewPointerObj(L, (void *)key, SWIGTYPE_p_kdb__Key, 1);
    return 2;
  }

  int _my_KeySet_ipairs(lua_State* L)
  {
    KeySet::iterator *it;
    const KeySet *ks;

    if (!SWIG_IsOK(SWIG_ConvertPtr(L, 1, (void **)&ks, SWIGTYPE_p_kdb__KeySet, 0)))
      SWIG_fail_ptr("ipairs", 1, SWIGTYPE_p_kdb__KeySet);

    lua_pushcfunction(L, _my_KeySet_ipairs_it); /* callback function */
    lua_pushlightuserdata(L, new KeySet::iterator(ks->begin())); /* param (the iterator) */
    lua_pushnumber(L, 0); /* start value (index param) */
    return 3;

  fail:
    lua_error(L);
    return 0;
  }
#endif

  int _my_KeySet_ipairs_it(lua_State* L)
  {
    const KeySet *ks;
    lua_Number i;

    if (!SWIG_IsOK(SWIG_ConvertPtr(L, 1, (void **)&ks, SWIGTYPE_p_kdb__KeySet, 0)))
      SWIG_fail_ptr("ipairs_it", 1, SWIGTYPE_p_kdb__KeySet);

    i = lua_tonumber(L, 2);
    lua_pop(L, 2);
    if (i == ks->end().base())
      return 0;

    lua_pushnumber(L, i + 1);
    SWIG_NewPointerObj(L, (void *)new kdb::Key(ks->begin()[i]), SWIGTYPE_p_kdb__Key, 1);
    return 2;

  fail:
    lua_error(L);
    return 0;
  }

  /* simple cursor variant: use the index param as cursor position */
  int _my_KeySet_ipairs(lua_State* L)
  {
    lua_pushcfunction(L, _my_KeySet_ipairs_it); /* callback function */
    lua_pushvalue(L, 1);  /* param (copy of Key) */
    lua_pushnumber(L, 0); /* start value (index param) */
    return 3;
  }
%}

%init %{
  SWIG_Lua_get_class_metatable(L, "KeySet");
  SWIG_Lua_add_function(L, "__ipairs", _my_KeySet_ipairs);
  lua_pop(L, 1);
%}

%luacode %{
  local orig_call = kdb.KeySet
  kdb.KeySet = function(alloc, ...)
    local ks = orig_call(alloc)

    if select("#", ...) > 0 then
      -- there's no need to check for KS_END
      -- ipairs will do this for us
      for _, arg in ipairs({...}) do
        ks:append(arg)
      end
    end

    return ks
  end

  local mt = getmetatable(kdb.KeySet())
  mt[".fn"]["lookup"] = function(self, ...)
    local key = self:_lookup(...)
    return not key:isNull() and key or nil
  end

  -- table-like key lookup
  -- NOTE: this also returns all available class methods
  -- e.g. ks["lookup"] will return above function
  local orig_index = mt["__index"]
  mt["__index"] = function(self, prop)
    local val = orig_index(self, prop)
    if val then
      return val
    end

    return self:lookup(prop)
  end
%}

%include "keyset.hpp"


/*
 * kdb.hpp
 */
// exception handling for kdb::KDB
%exception {
  KDB_CATCH(KDB_EXCEPTIONS)
}

%include "kdb.hpp"

// clear exception handler
%exception;
