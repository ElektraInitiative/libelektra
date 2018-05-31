/**
 * @file
 *
 * @brief kdb find operation results
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import {
  FIND_KEY_REQUEST, FIND_KEY_SUCCESS, FIND_KEY_FAILURE, CLEAR_SEARCH,
} from '../actions'

const initialState = {
  loading: false,
  error: false,
  done: false,
  results: [],
}

export default function batchUndoReducer (state = initialState, action) {
  switch (action.type) {
    case FIND_KEY_REQUEST:
      return { loading: true, error: false, done: false, results: [], query: action.query }

    case FIND_KEY_SUCCESS:
      return { loading: false, error: false, done: true, results: action.result.result, query: action.query }

    case FIND_KEY_FAILURE:
      return { loading: false, error: action.error, done: true, results: [], query: action.query }

    case CLEAR_SEARCH:
      return initialState

    default:
      return state
  }
}
