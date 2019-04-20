def optarg(info):
	val = info["default"]
	if val == "true":
		return '"false"'
	elif val == "false":
		return '"true"'
	else:
		return "optarg"
