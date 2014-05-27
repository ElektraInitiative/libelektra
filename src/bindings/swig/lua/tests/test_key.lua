require("kdb")

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
assert(swig_type(key)  == "kdb::Key *")
assert(swig_type(bkey) == "kdb::Key *")

local k = kdb.Key()
assert(swig_type(k) == "kdb::Key *")
assert(k:isValid() == false)

local k = kdb.Key("user/foo")
assert(swig_type(k) == "kdb::Key *")
assert(k:isValid() == true)

local k = kdb.Key(key)
assert(swig_type(k) == "kdb::Key *")
assert(k:isValid() == true)

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
assert(k:isBinary() == false)
assert(k:getMeta("binary") == nil)

k.name     = "system/key2"
k.basename = "key3"
k.binary   = "bvalue\0\0"
assert(k.name   == "system/key3")
assert(k.value  == "bvalue\0\0")
assert(k.binary == "bvalue\0\0")
assert(k:isBinary() == true)
assert(swig_type(k:getMeta("binary")) == "kdb::Key *")

local k = kdb.Key("user/key2")
assert(pcall(function() k.name = "foo" end) == false)

-- functions
assert(key:isNull()  == false)
assert(bkey:isNull() == false)

assert(key:isUser()    == true)
assert(bkey:isSystem() == true)
assert(key:isString()  == true)
assert(bkey:isBinary() == true)
assert(kdb.Key("user/foo"):isBelow(key))

local k = kdb.Key("user/key1", kdb.KEY_VALUE, "value")
assert(k:get(), "value")
k.binary = "bvalue\0\0"
assert(k:get(), "bvalue\0\0")

-- meta
assert(swig_type(key:getMeta("owner")) == "kdb::Key *")
assert(key:getMeta("owner").value   == "myowner")
assert(key:getMeta("owner").name    == "owner")
assert(key:getMeta("comment").value == "mycomment")
assert(key:getMeta("uid").value     == "123")
assert(key:getMeta("gid").value     == "456")
assert(key:getMeta("mode").value    == "755")
assert(key:getMeta("atime").value   == "123")
assert(key:getMeta("mtime").value   == "456")
assert(key:getMeta("ctime").value   == "789")
assert(key:getMeta("by").value      == "manuel")

assert(key:hasMeta("doesnt_exist") == false)
assert(key:getMeta("doesnt_exist") == nil)
assert(bkey:getMeta("binary"):isNull() == false)
assert(bkey:getMeta("owner")       == nil)

local k = kdb.Key("user/key1")
k:setMeta("foo", "bar")
assert(k:getMeta("foo").value == "bar")

function item_cnt(...)
	local cnt = 0
	for i, v in ... do
		cnt = cnt + 1
	end
	return cnt
end
assert(item_cnt(key:getMeta())  == 9)
assert(item_cnt(bkey:getMeta()) == 1)
