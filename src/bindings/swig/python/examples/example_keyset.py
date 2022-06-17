import kdb

ks1 = kdb.KeySet(100,
	kdb.Key("user:/key1"),
	kdb.Key("user:/key2"),
	kdb.Key("user:/key3")
	)

print("KeySet1 has {0} keys".format(len(ks1)))
print("")

print("We can add a key at runtime")
new_key = kdb.Key("user:/key4", "key_value")
ks1.append(new_key)
print("or even a list of keys")
list_of_keys = [kdb.Key("user:/key5", "key_value"), kdb.Key("user:/key6", "key_value")]
ks1.extend(list_of_keys)
print("")

print("If you want to remove some keys")
ks1.remove("user:/key4")
ks1.remove("user:/key5")
ks1.remove("user:/key6")
print("")

print("Alternatively you can use cut")
ks1.append(kdb.Key("user:/keytocut"))
ks1.append(kdb.Key("user:/keytocut/below"))
# cut(kdb::Key) removes all Keys below a given Key from the KeySet and returns them in a new KeySet
# this method takes a kdb.Key as input parameter in contrast to remove() which takes the path as a string
print(ks1.cut(kdb.Key("user:/keytocut")))
print("")

print("We can easily iterate over the keyset to check out its content:")
for k in ks1:
	print("  {0}".format(k))
print("")

print("This works the other direction too:")
for k in reversed(ks1):
	print("  {0}".format(k))
print("")

print("We can check if a key is in a keyset:")
print("  Is user:/key1 in KeySet1? {0}".format("user:/key1" in ks1))
print("This works with Key objects too:")
print("  Is Key(system:/key1) in KeySet1? {0}".format("system:/key1" in ks1))
print("")

print("Index access is supported as well:")
print("  KeySet1[1]={0}".format(ks1[1]))
print("  KeySet1[-1]={0}".format(ks1[-1]))
print("  KeySet1['user:/key1']={0}".format(ks1["user:/key1"]))
try:
	print("  KeySet1['doesnt_exist']={0}".format(ks1["doesnt_exist"]))
except KeyError:
	print("  KeySet1['doesnt_exist'] throws KeyError")
print("  KeySet1.lookup('doesnt_exist')={0}".format(ks1.lookup("doesnt_exist")))
print("")

print("You asked for slices? You get slices:")
print("  KeySet1[1:3]={0}".format([ str(k) for k in ks1[1:3] ]))
print("")

print("We can create shallow copies and remove keys without affecting other keysets:")
ks1.append(kdb.Key("user:/extraKey"))
ks2 = kdb.KeySet(ks1.dup())
# or ks2 = copy.copy(ks1)
ks2.remove("user:/extraKey")
print("  KeySet2 now has {0} keys while KeySet1 still has {1} keys".format(len(ks2), len(ks1)))

print("In Python we can even create deep copies and modify the keys inside:")
ks1[0].setMeta("foo", "bar")
import copy
ks2 = copy.deepcopy(ks1)
ks2[0].setMeta("foo", "changed")
print("  KeySet1[0].getMeta('foo').value={0}".format(ks1[0].getMeta("foo").value))
print("  KeySet2[0].getMeta('foo').value={0}".format(ks2[0].getMeta("foo").value))
print("")
