/**
 * @file
 *
 * @brief kdb find operation results
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import {
  FIND_KEY_REQUEST,
  FIND_KEY_SUCCESS,
  FIND_KEY_FAILURE,
  CLEAR_SEARCH,
  CLEAR_SEARCH_FINAL,
  DELETE_KEY_SUCCESS,
} from "../actions";

const initialState = {
  clearing: false,
  loading: false,
  error: false,
  done: false,
  results: [],
};

export default function batchUndoReducer(state = initialState, action) {
  switch (action.type) {
    case FIND_KEY_REQUEST:
      return {
        clearing: false,
        loading: true,
        error: false,
        done: false,
        results: [],
        query: action.query,
      };

    case FIND_KEY_SUCCESS:
      return {
        clearing: false,
        loading: false,
        error: false,
        done: true,
        results: action.result.result,
        query: action.query,
      };

    case FIND_KEY_FAILURE:
      return {
        clearing: false,
        loading: false,
        error: action.error,
        done: true,
        results: [],
        query: action.query,
      };

    case CLEAR_SEARCH:
      return { ...initialState, clearing: true };

    case CLEAR_SEARCH_FINAL:
      return initialState;

    case DELETE_KEY_SUCCESS: {
      const { path } = action.request;
      return {
        ...state,
        results: state.results.filter((p) => !p.startsWith(path)),
      };
    }

    default:
      return state;
  }
}
