class ElektraPlugin(object):
  def __init__(self):
    print("[CLASS-PYTHON-1] __init__")
    self.test = 0

  def open(self, errorKey):
    print("[CLASS-PYTHON-1] open -->")
    return 0

  def get(self, returned, parentKey):
    print("[CLASS-PYTHON-1] get %s test=%d" % (parentKey, self.test))
    self.test = self.test + 1
    return 1

  def set(self, returned, parentKey):
    print("[CLASS-PYTHON-1] set")
    return 1

  def error(self, returned, parentKey):
    print("[CLASS-PYTHON-1] error")
    return 1

  def close(self, errorKey):
    print("[CLASS-PYTHON-1] <-- close")
    print("errorKey={0}".format(repr(errorKey)))
    print(errorKey.name)
    errorKey.name="user/bar"
    return 0

print("[PYTHON-1] this is main. will be called on import")
