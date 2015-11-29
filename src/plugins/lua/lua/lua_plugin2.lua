local x = 1

function elektraOpen(config, errorKey)
	print("[LUA-2] open -->")
	x = x + 1
	return 0
end

function elektraGet(returned, parentKey)
	print("[LUA-2] get")
	return 1
end

function elektraSet(returned, parentKey)
	print("[LUA-2] set")
	return 1
end

function elektraError(returned, parentKey)
	print("[LUA-2] error")
	return 1
end

function elektraClose(errorKey)
	print("[LUA-2] <-- close")
	return 0
end
