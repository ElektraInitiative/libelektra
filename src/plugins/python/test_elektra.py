def elektraOpen(errorKey):
  print("[PYTHON-1] open -->")
  print("errorKey={0}".format(errorKey))
  return 0

def elektraClose(errorKey):
  print("[PYTHON-1] <-- close")
  print("errorKey={0}".format(repr(errorKey)))
  print(errorKey.name)
  errorKey.name="user/bar"
  return 0

print("[PYTHON-1] this is main. will be called on import")
