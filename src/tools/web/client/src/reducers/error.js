/**
 * @file
 *
 * @brief handle errors that happened in actions
 *
 * they will be shown in a small notification at the bottom of the UI
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import {
  INSTANCES_FAILURE, INSTANCE_UPDATE_FAILURE, INSTANCE_DELETE_FAILURE,
  CREATE_INSTANCE_FAILURE,
  GET_KEY_FAILURE, SET_KEY_FAILURE,
} from '../actions'

export default function errorReducer (state = false, action) {
  switch (action.type) {
    case INSTANCES_FAILURE:
    case INSTANCE_UPDATE_FAILURE:
    case INSTANCE_DELETE_FAILURE:
    case CREATE_INSTANCE_FAILURE:
    case GET_KEY_FAILURE:
    case SET_KEY_FAILURE:
      return action.error

    default:
      return state
  }
}
