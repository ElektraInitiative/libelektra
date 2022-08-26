%module merge

%include "../common.i"

%include <stl.i>
%include "../common.i"
%feature("autodoc", "3");

%import "kdb.i"

%{
#include "kdbmerge.h"
%}


ckdb::KeySet * elektraMerge (ckdb::KeySet * our, ckdb::Key * ourRoot, ckdb::KeySet * their, ckdb::Key * theirRoot, ckdb::KeySet * base, ckdb::Key * baseRoot, ckdb::Key * resultKey,
		       int strategy, ckdb::Key * informationKey);
int getConflicts (ckdb::Key * informationKey);

%pythoncode {
class MergeKeys:
  def __init__(self, keys, parentKey):
    self.keys = keys
    self.root = parentKey

class MergeResult:
  def __init__(self, mergedKeys, informationKey):
    if mergedKeys is None:
       self.mergedKeys = None
    else:
       self.mergedKeys = kdb.KeySet(mergedKeys)

    self.mergeInformation = informationKey

  def hasConflicts(self):
    return self.mergedKeys is None or getConflicts(self.mergeInformation.getKey()) > 0

class ConflictStrategyAbort:
  def getStrategyNumber(self):
    return 1

class ConflictStrategyOur:
  def getStrategyNumber(self):
    return 3

class ConflictStrategyTheir:
  def getStrategyNumber(self):
    return 4

class Merger:
  def merge(self, base, ours, theirs, root, conflictStrategy):
    informationKey = kdb.Key()
    res = elektraMerge(ours.keys.getKeySet(), ours.root.getKey(), theirs.keys.getKeySet(), theirs.root.getKey(), base.keys.getKeySet(), base.root.getKey(), root.getKey(), conflictStrategy.getStrategyNumber(), informationKey.getKey())
    return MergeResult(res, informationKey)
};