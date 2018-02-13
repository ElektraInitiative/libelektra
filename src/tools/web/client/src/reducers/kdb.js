/**
 * @file
 *
 * @brief handle actions related to operations on the Elektra key database (kdb)
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import {
  GET_KEY_SUCCESS, SET_KEY_REQUEST, DELETE_KEY_REQUEST, MOVE_KEY_SUCCESS,
  SET_META_REQUEST, DELETE_META_REQUEST, COPY_KEY_REQUEST,
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

    // TODO: recursively copy keys here? is this needed? we refresh on expand anyway
    case COPY_KEY_REQUEST: {
      const { id, from, to } = action && action.request
      const fromData = state[id] && state[id][from]
      return updateState(state, { ...fromData, id, path: to })
    }

    case SET_META_REQUEST: {
      const { id, path, key, value } = action.request
      const { meta } = state[id] && state[id][path]
      return updateState(state, { id, path, meta: { ...meta, [key]: value } })
    }

    case DELETE_META_REQUEST: {
      const { id, path, key } = action.request
      const { meta } = state[id] && state[id][path]
      return updateState(state, { id, path, meta: { ...meta, [key]: undefined } })
    }


    // TODO: recursively delete keys here? is this needed? we refresh on expand anyway
    case DELETE_KEY_REQUEST: {
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
    }

    // TODO: recursively move keys here? is this needed? we refresh on expand anyway
    case MOVE_KEY_SUCCESS: {
      const { id, from, to } = action && action.request
      const fromData = state[id] && state[id][from]
      return {
        ...state,
        [id]: state[id] && Object.keys(state[id]).reduce(
          (res, key) => {
            if (key !== from) { // exclude `from` key
              if (key === to) { // move data to `to` key
                res[key] = fromData
              } else { // otherwise, copy from state
                res[key] = state[id][key]
              }
            }
            return res
          }, {}
        ),
      }
    }

    // TODO: specifically handle failure (show error inline)
    default:
      return state
  }
}
