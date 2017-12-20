/**
 * @file
 *
 * @brief handle actions related to operations on the Elektra key database (kdb)
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import {
  GET_KEY_SUCCESS, SET_KEY_REQUEST, DELETE_KEY_REQUEST,
} from '../actions'

const updateState = (state, { id, path, value, meta }) => {
  const updatedPart = {
    value: typeof value !== 'undefined'
      ? value
      : (state[id] && state[id][path] && state[id][path].value),
    meta: typeof meta !== 'undefined'
      ? meta
      : (state[id] && state[id][path] && state[id][path].meta),
  }
  return {
    ...state,
    [id]: {
      ...state[id],
      [path]: updatedPart,
    },
  }
}

// controls the state of the local kdb cache
export default function keyReducer (state = {}, action) {
  switch (action.type) {
    case GET_KEY_SUCCESS:
      return updateState(state, action.result)

    case SET_KEY_REQUEST:
      return updateState(state, action.request)

    case DELETE_KEY_REQUEST:
      const { id, path } = action.request
      return {
        ...state,
        [id]: state[id] && Object.keys(state[id]).reduce(
          (res, key) => {
            if (key !== path) {
              res[key] = state[id][key]
            }
            return res
          }, {}
        ),
      }

    // TODO: specifically handle failure (show error inline)
    default:
      return state
  }
}
