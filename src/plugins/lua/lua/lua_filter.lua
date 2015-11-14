local x = 1

function elektraOpen(errorKey)
	print("[LUA-FILTER] open -->")
	x = x + 1
	return 0
end

function elektraGet(returned, parentKey)
	print(parentKey.name)
	mod = "system/elektra/modules/lua"
	if parentKey.name == mod then
		print("[LUA-FILTER] contract")
		returned.append(kdb.Key(mod, kdb.KEY_VALUE, "contract below"))
		returned.append(kdb.Key(mod+"/infos", kdb.KEY_VALUE, "infos below"))
		returned.append(kdb.Key(mod+"/infos/provides", kdb.KEY_VALUE, "filter"))
		returned.append(kdb.Key(mod+"/infos/placements", kdb.KEY_VALUE, "postgetstorage postcommit"))
		return 1
	end
	print("[LUA-FILTER] get")
	return 1
end

function elektraSet(returned, parentKey)
	print("[LUA-FILTER] set")
	return 1
end

function elektraError(returned, parentKey)
	print("[LUA-FILTER] error")
	return 1
end

function elektraClose(errorKey)
	print("[LUA-FILTER] <-- close")
	return 0
end
