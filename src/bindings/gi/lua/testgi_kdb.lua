lgi = require('lgi')
kdb = lgi.require('GElektra')

TEST_NS = "user/tests/gi_lua"

-- kdbconfig.h
assert(type(kdb.DB_SYSTEM) == "string")
assert(type(kdb.DB_USER)   == "string")
assert(type(kdb.DB_HOME)   == "string")
assert(type(kdb.DEBUG)     == "number")

-- kdb.h
assert(type(kdb.VERSION) == "string")
assert(type(kdb.VERSION_MAJOR) == "number")
assert(type(kdb.VERSION_MINOR) == "number")
assert(type(kdb.VERSION_MICRO) == "number")
assert(kdb.KS_END == nil)

-- ctor
assert(kdb.KDB:is_type_of(kdb.KDB()))
local error = kdb.Key()
assert(kdb.KDB:is_type_of(kdb.KDB(error)))

-- get
do
	local db = kdb.KDB()
	local ks = kdb.KeySet(100)
	db:get(ks, "system/elektra")

	if os.getenv("CHECK_VERSION") == nil then
		local key = ks:lookup("system/elektra/version/constants/KDB_VERSION")
		assert(key.value == kdb.VERSION)
	end
end

-- set
do
	local db = kdb.KDB()
	local ks = kdb.KeySet(100)
	db:get(ks, TEST_NS)

	local key = ks:lookup(TEST_NS .. "/mykey")
	if not key then
		key = kdb.Key(TEST_NS .. "/mykey")
		ks:append(key)
	end
	key.string = "new_value"

	db:set(ks, TEST_NS)
end

do
	local db = kdb.KDB()
	local ks = kdb.KeySet(100)
	db:get(ks, TEST_NS)
	assert(ks:lookup(TEST_NS .. "/mykey").value == "new_value")
end

-- cleanup
do
	local db = kdb.KDB()
	local ks = kdb.KeySet(100)
	db:get(ks, TEST_NS)
	ks:cut(kdb.Key(TEST_NS))
	db:set(ks, TEST_NS)
end
