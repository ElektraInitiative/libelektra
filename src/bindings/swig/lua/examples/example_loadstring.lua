require("kdb")

i=1

local ks = kdb.KeySet(1, kdb.Key("user:/key1", kdb.KEY_VALUE, "i=2"))
s = ks["user:/key1"].value
x = loadstring(s)

x()
print(i)
