/**
 * @file
 *
 * @brief undo multiple actions at once
 *
 * used to "abort" changes in dialogs
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import {
  RESET_BATCH_UNDO,
  SET_KEY_SUCCESS,
  DELETE_KEY_SUCCESS,
  CREATE_KEY_SUCCESS,
  SET_META_SUCCESS,
  DELETE_META_SUCCESS,
  CREATE_META_SUCCESS,
  COPY_KEY_SUCCESS,
  MOVE_KEY_SUCCESS,
} from "../actions";

export default function batchUndoReducer(state = 0, action) {
  switch (action.type) {
    case RESET_BATCH_UNDO:
      return 0;

    case SET_KEY_SUCCESS:
    case DELETE_KEY_SUCCESS:
    case CREATE_KEY_SUCCESS:
    case SET_META_SUCCESS:
    case DELETE_META_SUCCESS:
    case CREATE_META_SUCCESS:
    case COPY_KEY_SUCCESS:
    case MOVE_KEY_SUCCESS:
      return state + 1;

    default:
      return state;
  }
}
