/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

%module kdb
#pragma SWIG nowarn=317 // Disable warning: Specialization of non-template

%include "../common.i"

%inline %{
  long ksSize (const ckdb::_KeySet * ks) {
  return ckdb::ksGetSize (ks);
}

ckdb::_Key * ksAt (ckdb::_KeySet * ks, ssize_t pos) {
  return ckdb::ksAtCursor (ks, pos);
}
%}

%fragment("LuaSTLIterator_T", "header") {
  namespace myswig
  {
    /* simple class holding a STL forward iterator */
    template<typename STLIterator,
      typename Reference = typename std::iterator_traits<STLIterator>::reference >
    class LuaSTLIterator_T
    {
    public:
      typedef STLIterator iterator;
      typedef Reference reference;

      LuaSTLIterator_T(iterator cur, iterator _begin, iterator _end)
        : current(cur), begin(_begin), end(_end)
      {}

      reference value()
      {
        return *current;
      }

      reference next()
      {
        reference cur = value();
        incr();
        return cur;
      }

      LuaSTLIterator_T *incr()
      {
        ++current;
        return this;
      }

      bool hasNext()
      {
        return (current != end);
      }

    private:
      iterator current;
      iterator begin;
      iterator end;
    };

    template<typename STLIterator>
    inline LuaSTLIterator_T<STLIterator> *
    make_lua_iterator(const STLIterator& current, const STLIterator& begin,
      const STLIterator& end)
    {
      return new LuaSTLIterator_T<STLIterator>(current, begin, end);
    }
  }
}

%fragment("LuaSTLIterator", "header") {
  namespace myswig {
    template <class Type> struct LuaSTLIterator { };
  }
}

/* macro generates a specialized class template (as fragment) containing
 * static methods required by the output typemap below
 */
#define LuaSTLIterator_frag(Type...) "LuaSTLIterator_" {Type}
%define %LuaSTLIterator(Type, IterFn...)
%fragment(LuaSTLIterator_frag(Type), "header", fragment="LuaSTLIterator", fragment="LuaSTLIterator_T") {
  namespace myswig {
    template <>  struct LuaSTLIterator< myswig::LuaSTLIterator_T< Type > > {
      typedef myswig::LuaSTLIterator_T< Type > luaiterator;

      static int gc(lua_State *L) {
        luaiterator *iter = *(luaiterator **)lua_touserdata(L, 1);
        delete iter;
        return 0;
      }

      static int iter(lua_State *L) {
        luaiterator *iter = *(luaiterator **)lua_touserdata(L, lua_upvalueindex(1));
        if (!iter->hasNext())
          return 0;
        IterFn(L, iter->value());
        iter->next();
        return 1;
      }
    };
  }
}

/* add fragment */
%fragment(LuaSTLIterator_frag(Type));

/* store iterator in userdata and output closure
 * with userdata/iterator as first upvalue
 */
%typemap(out) myswig::LuaSTLIterator_T< Type > * {
  typedef $1_basetype luaiterator;

  luaiterator **iter = (luaiterator **)lua_newuserdata(L, sizeof(luaiterator *));
  *iter = $1;

  /* create and set metatable */
  lua_newtable(L);
  lua_pushcfunction(L, myswig::LuaSTLIterator< luaiterator >::gc);
  lua_setfield(L, -2, "__gc");
  lua_setmetatable(L, -2);

  lua_pushcclosure(L, myswig::LuaSTLIterator< luaiterator >::iter, 1);
  SWIG_arg++;
}
%enddef


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

%ignore kdb::Key::getName;
%ignore kdb::Key::setName;
%ignore kdb::Key::getBaseName;
%ignore kdb::Key::setBaseName;

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


%fragment("LuaSTLIterator_T");
%{
  inline static void name_iterator_iter(lua_State *L,
    myswig::LuaSTLIterator_T<kdb::Key::iterator>::reference val)
  {
    lua_pushlstring(L, val.data(), val.size());
  }
%}
%LuaSTLIterator(kdb::Key::iterator, name_iterator_iter);
%LuaSTLIterator(kdb::Key::reverse_iterator, name_iterator_iter);




%extend kdb::Key {
  Key(const char *name, int flags = 0) {
    return new kdb::Key(name,
      KEY_FLAGS, flags,
      KEY_END);
  }

  bool isNull() {
    return !bool(*self);
  }

  std::string __tostring() {
    return self->getName();
  }

  ckdb::_KeySet *__meta__() {
	  return ckdb::keyMeta($self->getKey());
  }

  myswig::LuaSTLIterator_T<kdb::Key::iterator> *name_iterator() {
    return myswig::make_lua_iterator(self->begin(), self->begin(), self->end());
  }

  myswig::LuaSTLIterator_T<kdb::Key::reverse_iterator> *reverse_name_iterator() {
    return myswig::make_lua_iterator(self->rbegin(), self->rbegin(), self->rend());
  }
};

%{
  /*
   * returns string or binary value depending on the type of the key
   */
  static int _my_Key_getValue(lua_State* L)
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
  kdb.Key = function(arg0, ...)
    local flags = 0
    local value = Nil
    local meta  = {}
    if select("#", ...) > 0 and swig_type(arg0) ~= "kdb.Key *" then
      local t = { ... }
      local i, arg = next(t, nil)
      while i do
        if arg == kdb.KEY_END then
          break
        elseif arg == kdb.KEY_SIZE then
          -- ignore value
          next(t, i)
        elseif arg == kdb.KEY_VALUE then
          i, value = next(t, i)
        elseif arg == kdb.KEY_FUNC then
          error("Unsupported meta type")
        elseif arg == kdb.KEY_FLAGS then
          i, flags = next(t, i)
        elseif arg == kdb.KEY_META then
          i, k = next(t, i)
          i, meta[k] = next(t, i)
        elseif type(arg) == "number" then
          io.stderr:write("Deprecated option in keyNew: ", arg, "\n")
          flags = bit32.bor(flags, arg)
        elseif kdb.DEBUG > 0 then
          io.stderr:write("Unknown option in keyNew: ", arg, "\n")
        end
        i, arg = next(t, i)
      end
    end

    -- make sure we call the proper constructor
    local key = flags > 0 and orig_call(arg0, flags)
      or arg0 and orig_call(arg0) or orig_call()

    if value then
      if key:isBinary() then
        key.binary = value
      else
        key.string = value
      end
    end
    for k, v in pairs(meta) do
      key:setMeta(k, v)
    end

    return key
  end

  local mt = getmetatable(kdb.Key())
  mt[".fn"]["__metaIter"] = function(self)
     return coroutine.wrap(
      function()
        local metaKeys = self:__meta__();
        local size = kdb.ksSize(metaKeys);
        if size > 0 then
          for i = 1, size do
            local meta = kdb.ksAt(metaKeys, i - 1)
            coroutine.yield(meta)
          end
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

// metadata
%template(_getMeta) kdb::Key::getMeta<const kdb::Key>;
%template(setMeta) kdb::Key::setMeta<const char *>;

// clear exception handler
%exception;


/*
 * keyset.hpp
 */
// exception handling for kdb::KeySet
%exception {
  KDB_CATCH(KEY_EXCEPTIONS)
}

%ignore kdb::KeySet::size;

%rename("_%s") kdb::KeySet::lookup;
%rename("_lookup") kdb::KeySet::at;

%fragment("LuaSTLIterator_T");
%{
  inline static void key_iterator_iter(lua_State *L,
    myswig::LuaSTLIterator_T<kdb::KeySet::iterator>::reference val)
  {
    SWIG_NewPointerObj(L, (void *)new kdb::Key(val), SWIGTYPE_p_kdb__Key, 1);
  }
%}
%LuaSTLIterator(kdb::KeySet::iterator, key_iterator_iter);

%extend kdb::KeySet {
  KeySet(size_t alloc) {
    return new kdb::KeySet(alloc, KS_END);
  }

  size_t __len(void *) {
    return self->size();
  }

  myswig::LuaSTLIterator_T<kdb::KeySet::iterator> *iterator() {
    return myswig::make_lua_iterator(self->begin(), self->begin(), self->end());
  }

  Key remove(const Key &k) {
    return self->lookup(k, KDB_O_POP);
  }

  Key remove(const std::string &name) {
    return self->lookup(name, KDB_O_POP);
  }
};

%luacode %{
  local orig_call = kdb.KeySet
  kdb.KeySet = function(alloc, ...)
    local ks = orig_call(alloc)

    if select("#", ...) > 0 then
      -- there is no need to check for KS_END
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

// clear exception handler
%exception;


/*
 * kdb.hpp
 */
// exception handling for kdb::KDB
%exception {
  KDB_CATCH(KDB_EXCEPTIONS)
}

%include "kdb.hpp"

%luacode %{
  local orig_call = kdb.goptsContract
  kdb.goptsContract = function(contract, args, env, parentKey, goptsConfig)
    local argsString = ""
    for i, arg in ipairs(args) do
      argsString = argsString .. arg .. "\0"
    end
    
    local envString = ""
    for i, e in ipairs(env) do
      envString = envString .. e .. "\0"
    end

    orig_call(contract, argsString, envString, parentKey, goptsConfig)
  end
%}

// clear exception handler
%exception;
