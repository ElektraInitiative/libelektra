import sys, os

py2dir = os.path.dirname(__file__)
pydir  = os.path.abspath(py2dir + "/../../python")
sys.path.append(pydir)

import importlib
py2module = os.path.splitext(os.path.basename(__file__))[0]
ElektraPlugin = importlib.import_module("python.%s" % py2module).ElektraPlugin
