/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

%module kdb

%include "../python/kdb.i"

/* python2 compat
%pythonbegin %{
  from __future__ import print_function
%}
*/

/*
 * python2 doesn't have a binary datatype
 * so we overwrite the __init__ to handle KEY_VALUE/BINARY
 */
%pythoncode %{
  Key.__oldinit__ = Key.__init__

  def __Key_new_init__(self, *args):
    passed = []
    value  = None

    if len(args):
      args = iter(args)
      passed.append(next(args))
      for arg in args:
        if arg == KEY_VALUE:
          value = next(args)
        else:
          passed.append(arg)

    self.__oldinit__(*passed)

    if value:
      if self.isBinary():
        self.binary = value
      else:
        self.string = value

  Key.__init__ = __Key_new_init__
%}

/* more "binary datatype missing"-stuff */
%pythoncode %{
  del Key.set
  Key.value  = property(Key.get, None, None, "Key value")
  Key.string = property(Key._getString, Key._setString, None, "Key string value")
  Key.binary = property(Key._getBinary, Key._setBinary, None, "Key binary value")
%}
