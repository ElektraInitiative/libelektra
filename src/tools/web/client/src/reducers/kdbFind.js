/**
 * @file
 *
 * @brief kdb find operation results
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import {
  FIND_KEY_REQUEST, FIND_KEY_SUCCESS, FIND_KEY_FAILURE
} from '../actions'

const initialState = {
  loading: false,
  error: false,
  results: [],
}

export default function batchUndoReducer (state = initialState, action) {
  switch (action.type) {
    case FIND_KEY_REQUEST:
      return { loading: true, error: false, results: [] }

    case FIND_KEY_SUCCESS:
      return { loading: false, error: false, results: action.result }

    case FIND_KEY_FAILURE:
      return { loading: false, error: action.error, results: [] }

    default:
      return state
  }
}
