require("kdb")

do
	-- open database
	local db = kdb.KDB()
	ks = kdb.KeySet(100)
	-- get keys
	db:get(ks, "user:/MyApp")

	-- check if key exists
	key = ks["user:/MyApp/mykey"]
	if not key then
		-- create a new key + append to keyset
		key = kdb.Key("user:/MyApp/mykey")
		ks:append(key)
	end
	-- change keys value
	key.string = "new_value"

	-- store new keyset
	db:set(ks, "user:/MyApp")

	-- database closes at end of context is implicit
end