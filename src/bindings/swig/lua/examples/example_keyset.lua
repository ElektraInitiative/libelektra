require("kdb")

orig_print = print
print = function(s, ...)
  return orig_print(s:format(...))
end

local ks1 = kdb.KeySet(100,
	kdb.Key("user:/key1"),
	kdb.Key("user:/key2"),
	kdb.Key("user:/key3")
	)

print("KeySet1 has %d keys", #ks1)
print("")

print("We can easily iterate over the keyset to check out its content:")
for i, k in ipairs(ks1) do
	print("  %s", k)
end
print("")

print("We can check if a key is in a keyset:")
print("  Is user:/key1 in KeySet1? %s", ks1["user:/key1"] ~= nil)
print("This works with Key objects too:")
print("  Is Key(system:/key1) in KeySet1? %s", ks1["system:/key1"] ~= nil)
print("")

print("Index access is supported as well:")
print("  KeySet1[1]=%s", ks1[1])
print("  KeySet1[-1]=%s", ks1[-1])
print("  KeySet1['user:/key1']=%s", ks1["user:/key1"])
print("  KeySet1['doesnt_exist']=%s", ks1["doesnt_exist"])
print("")

print("We can create shallow copies and remove keys without affecting other keysets:")
local ks2 = kdb.KeySet(ks1:dup())
ks2:pop()
print("  KeySet2 now has %d keys while KeySet1 still has %d keys", #ks2, #ks1)
print("")
