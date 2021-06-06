/**
 * @file
 *
 * @brief handle errors that happened in actions
 *
 * they will be shown in a small snackbar at the bottom of the UI
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import {
  INSTANCES_FAILURE,
  INSTANCE_UPDATE_FAILURE,
  INSTANCE_DELETE_FAILURE,
  CREATE_INSTANCE_FAILURE,
  GET_KEY_FAILURE,
  SET_KEY_FAILURE,
  CREATE_KEY_FAILURE,
  DISMISS_ERROR,
  GET_KDB_FAILURE,
  GET_KDB_SUCCESS,
} from "../actions";

export default function errorReducer(state = false, action) {
  switch (action.type) {
    case GET_KDB_FAILURE:
      return { ...action.error, instanceError: true };

    case GET_KDB_SUCCESS:
      return false;

    case INSTANCES_FAILURE:
    case INSTANCE_UPDATE_FAILURE:
    case INSTANCE_DELETE_FAILURE:
    case CREATE_INSTANCE_FAILURE:
    case GET_KEY_FAILURE:
    case SET_KEY_FAILURE:
    case CREATE_KEY_FAILURE:
      const { message } = action.error;
      if (message.includes("ECONNREFUSED")) {
        // elektrad was shut down -> instance error
        return { ...action.error, instanceError: true };
      }
      return action.error;

    case DISMISS_ERROR:
      return false;

    default:
      return state;
  }
}
