/**
 * @file
 *
 * @brief this combines all reducers into one tree
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { combineReducers } from "redux";
import { undoHistoryReducer } from "redux-undo-redo-middleware";

import { reducer as idleReducer } from "redux-promises";

import instancesReducer from "./instances";
import containerReducer from "./container";
import keyReducer from "./kdb";
import pathReducer from "./ls";
import errorReducer from "./error";
import notificationReducer from "./notification";
import batchUndoReducer from "./batchUndo";
import kdbFindReducer from "./kdbFind";

export default combineReducers({
  idle: idleReducer,
  instances: instancesReducer,
  container: containerReducer,
  kdb: keyReducer,
  ls: pathReducer,
  error: errorReducer,
  notification: notificationReducer,
  undoHistory: undoHistoryReducer,
  batchUndo: batchUndoReducer,
  kdbFind: kdbFindReducer,
});
