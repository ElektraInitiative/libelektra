import sys, os

py2dir = os.path.dirname(__file__)
pydir  = os.path.abspath(py2dir + "/../../python")
sys.path.append(pydir)

import importlib
py2module = os.path.splitext(os.path.basename(__file__))[0]
ElektraPluginFoo = importlib.import_module("python.%s" % py2module).ElektraPluginFoo
