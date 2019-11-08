local x = 1

function elektraOpen(config, errorKey)
	print("[LUA-FILTER] open " .. x)
	x = x + 1
	return 0
end

function elektraGet(returned, parentKey)
	print(parentKey.name)
	mod = "system:/elektra/modules/lua"
	if parentKey.name == mod then
		returned:append(kdb.Key(mod, kdb.KEY_VALUE, "contract below"))
		returned:append(kdb.Key(mod.."/infos", kdb.KEY_VALUE, "infos below"))
		returned:append(kdb.Key(mod.."/infos/provides", kdb.KEY_VALUE, "filter"))
		returned:append(kdb.Key(mod.."/infos/description", kdb.KEY_VALUE, "Prints [LUA-FILTER] <fun> <num> when executed "))
		returned:append(kdb.Key(mod.."/infos/placements", kdb.KEY_VALUE, "postgetstorage postcommit"))
		-- for i, k in ipairs(returned) do
		-- 	print(i, k, k.value)
		-- end
	end
	print("[LUA-FILTER] get " .. x)
	x = x + 1
	return 1
end

function elektraSet(returned, parentKey)
	print("[LUA-FILTER] set " .. x)
	x = x + 1
	return 1
end

function elektraError(returned, parentKey)
	print("[LUA-FILTER] error " .. x)
	x = x + 1
	return 1
end

function elektraClose(errorKey)
	print("[LUA-FILTER] close " .. x)
	x = x + 1
	return 0
end
