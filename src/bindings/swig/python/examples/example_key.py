import kdb

key1 = kdb.Key("user:/key1", kdb.KEY_VALUE, "some_value")
print("Key1 name=\"{0}\" value=\"{1}\"".format(key1.name, key1.value))
print("")

print("Every Key has properties. Some are read only, some are read+write.")
print("Properties of Key1:")
print("  key1.name     = \"{0}\"".format(key1.name))
print("  key1.value    = \"{0}\"".format(key1.value))
print("  key1.basename = \"{0}\"".format(key1.basename))
print("")

key1.value = b"some\0value\0"
print("Key1 is now binary: {0}".format(key1.isBinary()))
print("")

key2 = kdb.Key(key1.dup())
# or key2 = copy.copy(key1)
print("Key2 is a copy of Key1. Do they match? {0}".format(key1 == key2))
print("")

key1.name = "system:/key1"
print("We changed name of Key1. New name is \"{0}\"".format(key1.name))
print("Do they still match? {0}".format(key1 == key2))
print("")

key1.basename = "key1_changed"
print("Changing the basename only is possible as well. New name is \"{0}\"".format(key1.name))
print("")

key1.setMeta("foo",     "bar")
key1.setMeta("owner",   "manuel")
key1.setMeta("comment/#0", "this is my example key")
print("Keys can have metadata. We can iterate over or fetch them by name.")
print("Meta data of Key1 with their values:")
for meta in key1.getMeta():
	print("  key1.{0} = \"{1}\"".format(meta.name, meta.value))
print("Remember: Metadata is returned as a Key object.")
print("")
