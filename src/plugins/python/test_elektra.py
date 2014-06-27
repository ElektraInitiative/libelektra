def elektraOpen(errorKey):
  print("[PYTHON-1] open -->")
  print("errorKey={0}".format(errorKey))
  return 0

def elektraGet(returned, parentKey):
  print("[PYTHON-1] get")
  return 1

def elektraSet(returned, parentKey):
  print("[PYTHON-1] set")
  return 1

def elektraError(returned, parentKey):
  print("[PYTHON-1] error")
  return 1

def elektraClose(errorKey):
  print("[PYTHON-1] <-- close")
  print("errorKey={0}".format(repr(errorKey)))
  print(errorKey.name)
  errorKey.name="user/bar"
  return 0

print("[PYTHON-1] this is main. will be called on import")
