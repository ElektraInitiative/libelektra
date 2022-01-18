require("kdb")

orig_print = print
print = function(s, ...)
  return orig_print(s:format(...))
end

local key1 = kdb.Key("user:/key1", kdb.KEY_VALUE, "some_value")
print("Key1 name=\"%s\" value=\"%s\"", key1.name, key1.value)
print("")

print("Every Key has properties. Some are read only, some are read+write.")
print("Properties of Key1:")
print("  key1.name     = \"%s\"", key1.name)
print("  key1.value    = \"%s\"", key1.value)
print("  key1.basename = \"%s\"", key1.basename)
print("  key1.fullname = \"%s\"", key1.fullname)
print("")

key1.binary = "some\0value\0"
print("Key1 is now binary: %s", key1:isBinary())
print("")

local key2 = kdb.Key(key1:dup())
print("Key2 is a copy of Key1. Do they match? %s", key1 == key2)
print("")

key1.name = "system:/key1"
print("We changed name of Key1. New name is \"%s\"", key1.name)
print("Do they still match? %s", key1 == key2)
print("")

key1.basename = "key1_changed"
print("Changing the basename only is possible as well. New name is \"%s\"", key1.name)
print("")

key1:setMeta("foo",     "bar")
key1:setMeta("owner",   "manuel")
key1:setMeta("comment/#0", "this is my example key")
print("Keys can have metadata. We can iterate over or fetch them by name.")
print("Meta data of Key1 with their value:")
for meta in key1:getMeta() do
	print("  key1.%s = \"%s\"", meta.name, meta.value)
end
print("Remember: Metadata is returned as a Key object.")
print("")
