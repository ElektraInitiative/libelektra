lgi = require('lgi')
kdb = lgi.require('GElektra')

local key = kdb.Key("user/key",
	kdb.KEY_NAME,    "user/foo/bar",
	kdb.KEY_VALUE,   "value",
	kdb.KEY_OWNER,   "myowner",
	kdb.KEY_COMMENT, "mycomment",
	kdb.KEY_UID,     "123",
	kdb.KEY_GID,     456,
	kdb.KEY_MODE,    tonumber(644, 8),
	kdb.KEY_ATIME,   123,
	kdb.KEY_MTIME,   "456",
	kdb.KEY_CTIME,   789,
	kdb.KEY_DIR,
	kdb.KEY_META,    "by", "manuel",
	kdb.KEY_NULL
	)

local bkey = kdb.Key("system/bkey",
	kdb.KEY_BINARY,
	kdb.KEY_VALUE,   "bvalue\0\0",
	kdb.KEY_END,
	kdb.KEY_OWNER,   "owner"
	)

-- ctor
assert(kdb.Key:is_type_of(key))
assert(kdb.Key:is_type_of(bkey))

local k = kdb.Key()
assert(kdb.Key:is_type_of(k))
assert(k:isvalid() == false)

local k = kdb.Key("user/foo")
assert(kdb.Key:is_type_of(k))
assert(k:isvalid() == true)

local k = kdb.Key(key)
assert(kdb.Key:is_type_of(k))
assert(k:isvalid() == true)

-- operator
assert(key ~= bkey)
assert(kdb.Key(key) == key)
assert(key == kdb.Key("user/foo/bar", kdb.KEY_OWNER, "myowner"))
assert(kdb.Key() == kdb.Key())
assert(kdb.Key("user/key1") ~= kdb.Key("user/key2"))

assert(kdb.Key("user/key1") == kdb.Key("user/key1"))
assert(kdb.Key("user/key1") ~= kdb.Key("user/key2"))
assert(kdb.Key("user/key1") <  kdb.Key("user/key2"))
assert(kdb.Key("user/key1") <= kdb.Key("user/key2"))
assert(kdb.Key("user/key2") >  kdb.Key("user/key1"))
assert(kdb.Key("user/key2") >= kdb.Key("user/key1"))

assert(tostring(key)  == "user/foo/bar")
assert(tostring(bkey) == "system/bkey")

-- properties
assert(key.name      == "user/foo/bar")
assert(key.value     == "value")
assert(key.string    == "value")
assert(key.basename  == "bar")
assert(key.dirname   == "user/foo")
assert(key.fullname  == "user:myowner/foo/bar")

assert(bkey.name     == "system/bkey")
assert(bkey.value    == "bvalue\0\0")
assert(bkey.binary   == "bvalue\0\0")
assert(bkey.basename == "bkey")
assert(bkey.dirname  == "system")
assert(bkey.fullname == "system/bkey")

local k = kdb.Key("user/key1", kdb.KEY_VALUE, "value")
assert(k:isbinary() == false)
assert(k:getmeta("binary") == nil)

k.name     = "system/key2"
k.basename = "key3"
k.binary   = "bvalue\0\0"
assert(k.name   == "system/key3")
assert(k.value  == "bvalue\0\0")
assert(k.binary == "bvalue\0\0")
assert(k:isbinary() == true)
assert(kdb.Key:is_type_of(k:getmeta("binary")))

local k = kdb.Key("user/key2")
assert(pcall(function() k.name = "foo" end) == false)

-- functions
assert(key:isnull()  == false)
assert(bkey:isnull() == false)

assert(key:isuser()    == true)
assert(bkey:issystem() == true)
assert(key:isstring()  == true)
assert(bkey:isbinary() == true)
assert(key:isbelow(kdb.Key("user/foo")))

local k = kdb.Key("user/key1", kdb.KEY_VALUE, "value")
assert(k:get(), "value")
k.binary = "bvalue\0\0"
assert(k:get(), "bvalue\0\0")

-- meta
assert(kdb.Key:is_type_of(key:getmeta("owner")))
assert(key:getmeta("owner").value   == "myowner")
assert(key:getmeta("owner").name    == "owner")
assert(key:getmeta("comment").value == "mycomment")
assert(key:getmeta("uid").value     == "123")
assert(key:getmeta("gid").value     == "456")
assert(key:getmeta("mode").value    == "755")
assert(key:getmeta("atime").value   == "123")
assert(key:getmeta("mtime").value   == "456")
assert(key:getmeta("ctime").value   == "789")
assert(key:getmeta("by").value      == "manuel")

assert(key:hasmeta("doesnt_exist") == false)
assert(key:getmeta("doesnt_exist") == nil)
assert(bkey:getmeta("binary"):isnull() == false)
assert(bkey:getmeta("owner")       == nil)

local k = kdb.Key("user/key1")
k:setmeta("foo", "bar")
assert(k:getmeta("foo").value == "bar")

function item_cnt(...)
	local cnt = 0
	for i, v in ... do
		cnt = cnt + 1
	end
	return cnt
end
assert(item_cnt(key:getmeta())  == 9)
assert(item_cnt(bkey:getmeta()) == 1)
