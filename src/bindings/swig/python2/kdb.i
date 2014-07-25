%module kdb

%include "../python3/kdb.i"

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
    passed  = []
    skipped = []

    if len(args):
      iargs = iter(args)
      passed.append(next(iargs))
      for arg in iargs:
        if arg == KEY_VALUE:
          skipped.append(arg)
          skipped.append(next(iargs))
        elif arg == KEY_BINARY:
          skipped.append(arg)
        else:
          passed.append(arg)

    self.__oldinit__(*passed)

    if len(skipped):
      iskipped = iter(skipped)
      for arg in iskipped:
        if arg == KEY_VALUE:
          if self.isBinary():
            self.binary = next(iskipped)
          else:
            self.string = next(iskipped)
        elif arg == KEY_BINARY:
          self.setMeta("binary", "")

  Key.__init__ = __Key_new_init__
%}

/* more "binary datatype missing"-stuff */
%pythoncode %{
  del Key.set
  Key.value  = property(Key.get, None, None, "Key value")
  Key.string = property(Key._getString, Key._setString, None, "Key string value")
  Key.binary = property(Key._getBinary, Key._setBinary, None, "Key binary value")
%}
