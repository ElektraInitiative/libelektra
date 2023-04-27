%module record

%include "../common.i"

%include <stl.i>
%include "../common.i"
%feature("autodoc", "3");

%import "kdb.i"

%{
#include "kdbrecord.h"
#include "kdbdiff.h"
%}

bool elektraRecordEnableRecording (ckdb::KDB * handle, ckdb::Key * parentKey, ckdb::Key * errorKey);
bool elektraRecordDisableRecording (ckdb::KDB * handle, ckdb::Key * errorKey);
bool elektraRecordClearSession (ckdb::KDB * handle, ckdb::Key * errorKey);

bool elektraRecordRecord (ckdb::KDB * handle, ckdb::KDB * sessionStorageHandle, ckdb::KeySet * newKeys, ckdb::Key * parentKey, ckdb::Key * errorKey);
bool elektraRecordUndo (ckdb::KDB * handle, ckdb::KDB * sessionStorageHandle, ckdb::Key * parentKey, ckdb::Key * errorKey);
bool elektraRecordRemoveKey (ckdb::KDB * handle, ckdb::Key * toRemove, ckdb::Key * errorKey);
bool elektraRecordIsActive (ckdb::KDB * handle);

%inline %{
// We need to wrap elektraRecordGetDiff, as the original version outputs the diff via a double pointer
// There is no good way to do that in SWIG, so wrap it and return NULL on error
ckdb::ElektraDiff * elektraRecordGetDiff (ckdb::KDB * handle, ckdb::Key * errorKey) {
  ckdb::ElektraDiff * out = NULL;
  bool result = elektraRecordGetDiff (handle, &out, errorKey);
  if (result) return out;

  if (out != NULL)
  {
    elektraDiffDel (out);
  }
  return NULL;
}
%}


%pythoncode {
class RecordUtil:
  def enable(handle: kdb.KDB, parent_key: kdb.Key, error_key: kdb.Key):
    return elektraRecordEnableRecording(handle.getKdb(), parent_key.getKey(), error_key.getKey())

  def disable(handle: kdb.KDB, error_key: kdb.Key):
    return elektraRecordDisableRecording(handle.getKdb(), error_key.getKey())

  def clear(handle: kdb.KDB, error_key: kdb.Key):
    return elektraRecordClearSession(handle.getKdb(), error_key.getKey())

  def is_active(handle: kdb.KDB):
    return elektraRecordIsActive(handle.getKdb())

  def get_diff(handle: kdb.KDB, error_key: kdb.Key):
    diff = elektraRecordGetDiff(handle.getKdb(), error_key.getKey())
    if diff is None:
      return None
    return kdb.ElektraDiff(diff)
}
