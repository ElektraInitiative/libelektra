local lgi = require 'lgi'
local GElektra = lgi.GElektra

local core = require 'lgi.core'

local function __func_hide(klass, old, new)
	klass._method[new] = klass._method[old]
	klass._method[old] = nil
end

-- we need to multiplex certain metamethods as the metatable
-- is shared among *all* classes / instances in lgi
local function __create_pseudomt(klass, funcs)
	local mt = getmetatable(klass)
	for _, func in pairs(funcs) do
		local orig_func = mt[func]
		mt[func] = function(self, ...)
			-- using rawget, as we also might wrap __index
			local _type = core.object.query(self, 'repo')
			local _mt = rawget(_type, "__mt")
			if _mt then
				local _hook = rawget(_mt, func)
				if _hook then
					return _hook(self, orig_func, ...)
				end
			end

			return (orig_func) and orig_func(self, ...)
				or error("attempt to call undefined metamethod")
		end
	end
end

-- make the enums global
for n in pairs(GElektra.KeySwitch) do
	if string.upper(n) == n then
		GElektra._enum['KEY_' .. n] = GElektra.KeySwitch[n]
	end
end

for n in pairs(GElektra.KdbOptions) do
	if string.upper(n) == n then
		GElektra._enum['KDB_O_' .. n] = GElektra.KdbOptions[n]
	end
end

GElektra.KS_END = nil

-- setup shared instance hooks
__create_pseudomt(GElektra.Key(), {
		"__eq", "__lt", "__le", "__tostring",
		"__len", "__ipairs",
		--"__index" --disabled, too slow and error-prone
	})

-- Key
-- constructor
__func_hide(GElektra.Key, 'gi_init', '_init')
__func_hide(GElektra.Key, 'gi_make', '_make')
function GElektra.Key:_new(arg0, ...)
	if GElektra.Key:is_type_of(arg0) then
		return GElektra.Key._method._make(arg0)
	end

	local key = GElektra.Key.new(self)
	if not arg0 then
		return key
	end

	local flags = 0
	local value = nil
	local meta  = {}
	local t = { ... }
	local i, arg = next(t, nil)
	while i do
		if arg == GElektra.KEY_END then
			break
		elseif arg == GElektra.KEY_SIZE then
			-- ignore value
			next(t, i)
		elseif arg == GElektra.KEY_VALUE then
			i, value = next(t, i)
		elseif arg == GElektra.KEY_FUNC then
			error("Unsupported meta type")
		elseif arg == GElektra.KEY_FLAGS then
			i, flags = next(t, i)
		elseif arg == GElektra.KEY_META then
			i, k = next(t, i)
			i, meta[k] = next(t, i)
		elseif type(arg) == "number" then
			io.stderr:write("Deprecated option in keyNew: ", arg, "\n")
			flags = bit32.bor(flags, arg)
		elseif GElektra.DEBUG > 0 then
			io.stderr:write("Unknown option in keyNew: ", arg, "\n")
		end
		i, arg = next(t, i)
	end

	-- _init clears our key
	GElektra.Key._method._init(key, arg0, flags, value, value)
	for k, v in pairs(meta) do
		key:setmeta(k, v)
	end
	return key
end

-- operators
__func_hide(GElektra.Key, 'cmp', '_cmp')
GElektra.Key.__mt = GElektra.Key.__mt or {}
function GElektra.Key.__mt:__eq(_, other)
	return GElektra.Key._method._cmp(self, other) == 0
end
function GElektra.Key.__mt:__lt(_, other)
	return GElektra.Key._method._cmp(self, other) < 0
end
function GElektra.Key.__mt:__le(_, other)
	return GElektra.Key._method._cmp(self, other) <= 0
end

function GElektra.Key.__mt:__tostring()
	return self.name
end

-- hide name manipulation functions. use attributes below
__func_hide(GElektra.Key, 'setname',         '_setname')
__func_hide(GElektra.Key, 'setbasename',     '_setbasename')
__func_hide(GElektra.Key, 'getnamesize',     '_getnamesize')
__func_hide(GElektra.Key, 'getbasenamesize', '_getbasenamesize')
__func_hide(GElektra.Key, 'getfullnamesize', '_getfullnamesize')
local keyinvalidname = "Invalid Keyname: keyname needs to start "
		.. "with user/ or system/";
function GElektra.Key:_setname(name)
	ret = GElektra.Key._method._setname(self, name)
	return (ret > 0) and ret or error(keyinvalidname)
end

function GElektra.Key:_setbasename(name)
	ret = GElektra.Key._method._setbasename(self, name)
	return (ret > 0) and ret or error(keyinvalidname)
end

function GElektra.Key:addbasename(name)
	ret = GElektra.Key._method.addbasename(self, name)
	return (ret > 0) and ret or error(keyinvalidname)
end

-- properties / attributes
GElektra.Key._attribute = {}
GElektra.Key._attribute.name = {
	get = function(self)
		return GElektra.Key:_access_property(self, self._property.name)
	end,
	set = GElektra.Key._setname
}
GElektra.Key._attribute.basename = {
	get = function(self)
		return GElektra.Key:_access_property(self, self._property.basename)
	end,
	set = GElektra.Key._setbasename
}
GElektra.Key._attribute.fullname = {
	get = function(self)
		return GElektra.Key:_access_property(self, self._property.fullname)
	end
}

-- hide value manipulation functions. use attributes below
__func_hide(GElektra.Key, 'gi_getstring', '_getstring')
__func_hide(GElektra.Key, 'gi_getbinary', '_getbinary')
__func_hide(GElektra.Key, 'setstring',    '_setstring')
__func_hide(GElektra.Key, 'setbinary',    '_setbinary')
__func_hide(GElektra.Key, 'getvaluesize', '_getvaluesize')
GElektra.Key._attribute.string = {
	get = GElektra.Key._method._getstring,
	set = GElektra.Key._method._setstring
}
GElektra.Key._attribute.binary = {
	get = function(self)
		return tostring(GElektra.Key._method._getbinary(self))
	end,
	set = GElektra.Key._method._setbinary
}
GElektra.Key._attribute.value = {
	get = function(self)
		return (self:isbinary()) and self.binary or self.string
	end
}
GElektra.Key.get = GElektra.Key._attribute.value.get

-- metadata functions
__func_hide(GElektra.Key, 'rewindmeta',  '_rewindmeta')
__func_hide(GElektra.Key, 'nextmeta',    '_nextmeta')
__func_hide(GElektra.Key, 'currentmeta', '_currentmeta')
function GElektra.Key:getmeta(...)
	if select("#", ...) > 0 then
		local meta = GElektra.Key._method.getmeta(self, ...)
		return meta and not meta:isnull() and meta or nil
	end
	return self:__metaiter()
end

function GElektra.Key:__metaiter()
	return coroutine.wrap(
		function()
			GElektra.Key._method._rewindmeta(self)
			local meta = GElektra.Key._method._nextmeta(self)
			while meta and not meta:isnull() do
				coroutine.yield(meta)
				meta = GElektra.Key._method._nextmeta(self)
			end
		end
	)
end

-- KeySet
-- constructor
function GElektra.KeySet:_new(arg0, ...)
	if GElektra.KeySet:is_type_of(arg0) then
		return GElektra.KeySet.dup(arg0)
	end

	local ks = GElektra.KeySet.new(self)
	if arg0 then
		GElektra.KeySet.resize(ks, arg0)
	end

	-- there's no need to check for KS_END
	-- ipairs will do this for us
	for _, arg in ipairs({...}) do
		ks:append(arg)
	end

	return ks
end

-- operators
__func_hide(GElektra.KeySet, 'len', '_len')
GElektra.KeySet.__mt = GElektra.KeySet.__mt or {}
function GElektra.KeySet.__mt:__len()
	return GElektra.KeySet._method._len(self)
end

function GElektra.KeySet.__mt:__index(index_func, prop)
	local key = index_func(self, "lookup")(self, prop)
	return key or index_func(self, prop)
end

__func_hide(GElektra.KeySet, 'rewind',   '_rewind')
__func_hide(GElektra.KeySet, 'next',     '_next')
__func_hide(GElektra.KeySet, 'current',  '_current')
__func_hide(GElektra.KeySet, 'atcursor', '_atcursor')
function GElektra.KeySet.__mt:__ipairs(x, y, z)
	local function ks_ipairs(ks, i)
		local key = ks:lookup(i)
		if key then
			i = i + 1
			return i, key
		end
		return nil
	end
	return ks_ipairs, self, 0
end

__func_hide(GElektra.KeySet, 'gi_append',        'append')
__func_hide(GElektra.KeySet, 'gi_append_keyset', '_append_keyset')
function GElektra.KeySet:append(data)
	if GElektra.KeySet:is_type_of(data) then
		return GElektra.KeySet._method._append_keyset(self, data)
	end
	return GElektra.KeySet._method.append(self, data)
end

__func_hide(GElektra.KeySet, 'lookup_byname', '_lookup_byname')
function GElektra.KeySet:lookup(name)
	local key = nil
	if GElektra.Key:is_type_of(name) then
		key = GElektra.KeySet._method.lookup(self, name, GElektra.KDB_O_NONE)
	elseif type(name) == "string" then
		key = GElektra.KeySet._method._lookup_byname(self, name,
			GElektra.KDB_O_NONE)
	elseif type(name) == "number" then
		key = GElektra.KeySet._method._atcursor(self, name)
	else
		error("Invalid argument type")
	end
	return key
end

-- Kdb
-- constructor
__func_hide(GElektra.Kdb, 'gi_open', 'open')
function GElektra.Kdb:_new(arg0)
	local kdb = GElektra.Kdb.new(self)
	GElektra.Kdb._method.open(kdb, arg0 or GElektra.Key())
	return kdb
end

-- basic methods
function GElektra.Kdb:get(ks, parent)
	if type(parent) == "string" then
		parent = GElektra.Key(parent)
	end
	return GElektra.Kdb._method.get(self, ks, parent)
end

function GElektra.Kdb:set(ks, parent)
	if type(parent) == "string" then
		parent = GElektra.Key(parent)
	end
	return GElektra.Kdb._method.set(self, ks, parent)
end

GElektra.KDB = GElektra.Kdb
