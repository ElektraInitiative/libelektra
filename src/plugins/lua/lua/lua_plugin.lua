require("kdb")

function elektraOpen(config, errorKey)
	print("[LUA-1] open -->")
	return 0
end

function elektraGet(returned, parentKey)
	print("[LUA-1] get")
	if parentKey.name == 'user:/from_c' then
			returned:append(kdb.Key("user:/from_lua"))
	end
	return 1
end

function elektraSet(returned, parentKey)
	print("[LUA-1] set")
	return 1
end

function elektraError(returned, parentKey)
	print("[LUA-1] error")
	return 1
end

function elektraClose(errorKey)
	print("[LUA-1] <-- close")
	return 0
end
