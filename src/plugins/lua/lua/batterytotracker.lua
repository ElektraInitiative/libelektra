local x = 1

function elektraOpen(config, errorKey)
	print("[LUA-TRACKER] open " .. x)
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
		returned:append(kdb.Key(mod.."/infos/description", kdb.KEY_VALUE, "transforms values from battery values (in %) to tracker values (in seconds)"))
		returned:append(kdb.Key(mod.."/infos/placements", kdb.KEY_VALUE, "presetstorage"))
		returned:append(kdb.Key(mod.."/infos/metadata", kdb.KEY_VALUE, "transform/batterytotracker"))
		-- for i, k in ipairs(returned) do
		-- 	print(i, k, k.value)
		-- end
	end
	print("[LUA-TRACKER] get " .. x)
	x = x + 1
	return 1
end

function elektraSet(returned, parentKey)
	print("[LUA-TRACKER] set " .. x)
	local cnt = 0
	local k1
	local k2
	for i, v in ipairs(returned) do
		cnt = cnt + 1
		-- print ("[LUA-TRACKER] set " .. v.name)
		local m = v:getMeta("transform/batterytotracker")
		if (m ~= nil) then
			print("[LUA-TRACKER] it " .. v.name .. " has metavalue " .. m.value)
			k1 = v.name
			k2 = parentKey.name .. "/" .. m.value
			-- v.value = "changed"
		end
	end

	if (k1 ~= nil and k2 ~= nil) then
		print ("values " .. k1 .. " " .. k2)
		local x1 = returned:lookup(k1)
		local x2 = returned:lookup(k2)
		if (x1 ~= nil and x2 ~= nil) then
			print ("transform " .. x1.name .. " to " .. x2.name)
			print ("with values " .. x1.value .. " and " .. x2.value)
			x1.string = x2.value
		end
	end
	return cnt
end

function elektraError(returned, parentKey)
	print("[LUA-TRACKER] error " .. x)
	x = x + 1
	return 1
end

function elektraClose(errorKey)
	print("[LUA-TRACKER] close " .. x)
	x = x + 1
	return 0
end
