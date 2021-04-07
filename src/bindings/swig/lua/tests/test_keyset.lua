require("kdb")

local ks = kdb.KeySet(100,
	kdb.Key("system:/key1"),
	kdb.Key("system:/key2"),
	kdb.Key("user:/key3"),
	kdb.Key("user:/key4"),
	kdb.KS_END,
	kdb.Key("user:/lost")
	)

-- ctor
assert(swig_type(ks) == "kdb::KeySet *")

local t = kdb.KeySet(0)
assert(swig_type(t) == "kdb::KeySet *")

local t = kdb.KeySet(ks)
assert(swig_type(t) == "kdb::KeySet *")

local t = kdb.KeySet(ks:dup())
assert(swig_type(t) == "kdb::KeySet *")
assert(#t           == #ks)
t:pop()
assert(#t           == #ks - 1)

-- operator
assert(ks ~= kdb.KeySet(0))

assert(#ks            == 4)
assert(#kdb.KeySet(0) == 0)

assert(ks[0] == kdb.Key("user:/key3"))
assert(ks[-1] == kdb.Key("system:/key2"))

assert(ks["user:/key3"] == kdb.Key("user:/key3"))
assert(ks["user:/doesnt_exist"] == nil)

assert(ks[kdb.Key("system:/key2")] == kdb.Key("system:/key2"))
assert(ks[kdb.Key("user:/doesnt_exist")] == nil)

assert(ks[0]:isNameLocked()  == true)
assert(ks[0]:isValueLocked() == false)
assert(ks[0]:isMetaLocked()  == false)

-- functions
assert(ks:lookup("user:/key3") == kdb.Key("user:/key3"))
assert(ks:lookup(kdb.Key("system:/key2")) == kdb.Key("system:/key2"))
assert(ks:lookup(0)  == kdb.Key("user:/key3"))
assert(ks:lookup(-1) == kdb.Key("system:/key2"))

local t = kdb.KeySet(0)
t:append(kdb.Key("user:/foo"))
t:append(kdb.Key("user:/bar"))
assert(#t == 2)

-- remove
local t = kdb.KeySet(0)
t:append(kdb.Key("user:/test1"))
t:append(kdb.Key("user:/test2"))
t:append(kdb.Key("user:/test3"))
assert(#t == 3)
assert(t:remove("user:/test2") == kdb.Key("user:/test2"))
assert(#t == 2)
assert(t:remove(kdb.Key("user:/test2")):isValid() == false)

-- iterator
function item_cnt(...)
	local cnt = 0
	for v in ... do
		cnt = cnt + 1
	end
	return cnt
end
assert(item_cnt(ks:iterator())            == 4)
assert(item_cnt(kdb.KeySet(0):iterator()) == 0)
