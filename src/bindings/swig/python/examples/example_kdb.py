import kdb

# open database
with kdb.KDB() as db:
	ks = kdb.KeySet(100)
	# get keys
	db.get(ks, "user/MyApp")

	# check if key exists
	key = ks["user/MyApp/mykey"]
	if not key:
		# create a new key + append to keyset
		key = kdb.Key("user/MyApp/mykey")
		ks.append(key)
	# change keys value
	key.value = "new_value"

	# store new keyset
	db.set(ks, "user/MyApp")

# database close is implicit