import kdb

# open database
with kdb.KDB() as db:
	ks = kdb.KeySet(100)
	# get keys
	db.get(ks, "user:/MyApp")

	# check if key exists
	try:
		key = ks["user:/MyApp/mykey"]
	except KeyError:
		# create a new key + append to keyset
		key = kdb.Key("user:/MyApp/mykey")
		ks.append(key)

	# change keys value
	key.value = "new_value"

	# store new keyset
	db.set(ks, "user:/MyApp")

# database close is implicit
