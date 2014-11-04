lgi = require('lgi')
kdb = lgi.require('GElektra')

-- kdbconfig.h
assert(type(kdb.DB_SYSTEM) == "string")
assert(type(kdb.DB_USER)   == "string")
assert(type(kdb.DB_HOME)   == "string")
assert(type(kdb.DEBUG)     == "number")
assert(type(kdb.VERBOSE)   == "number")

-- kdb.h
assert(type(kdb.VERSION) == "string")
assert(type(kdb.VERSION_MAJOR) == "number")
assert(type(kdb.VERSION_MINOR) == "number")
assert(type(kdb.VERSION_MICRO) == "number")
assert(kdb.KS_END == nil)

--[[
-- disabled until elektra libraries can be loaded by
-- the build system (and static build is gone)

-- ctor
assert(kdb.KDB:is_type_of(kdb.KDB()))
error = kdb.Key()
assert(kdb.KDB:is_type_of(kdb.KDB(error)))

-- get
do
	local db = kdb.KDB()
	local ks = kdb.KeySet(100)
	db:get(ks, "system/elektra")

	key = ks:lookup("system/elektra/version/constants/KDB_VERSION")
	assert(key.value == kdb.VERSION)
end

-- set
do
	local db = kdb.KDB()
	ks = kdb.KeySet(100)
	db:get(ks, "user/MyApp")

	key = ks:lookup("user/MyApp/mykey")
	if not key then
		key = kdb.Key("user/MyApp/mykey")
		ks:append(key)
	end
	key.string = "new_value"

	db:set(ks, "user/MyApp")
end

do
	local db = kdb.KDB()
	ks = kdb.KeySet(100)
	db:get(ks, "user/MyApp")
	assert(ks:lookup("user/MyApp/mykey").value == "new_value")
end
]]--
