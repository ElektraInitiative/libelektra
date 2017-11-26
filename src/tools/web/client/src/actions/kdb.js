/**
 * @file
 *
 * @brief Elektra key database (kdb) specific actions, used in the tree view
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { thunkCreator, encodePath, parseJSONResponse } from './utils'

// ~~~

export const GET_KEY_REQUEST = 'GET_KEY_REQUEST'
export const GET_KEY_SUCCESS = 'GET_KEY_SUCCESS'
export const GET_KEY_FAILURE = 'GET_KEY_FAILURE'

export const getKey = (id, path, cluster = false) => thunkCreator({
  types: [GET_KEY_REQUEST, GET_KEY_SUCCESS, GET_KEY_FAILURE],
  promise: fetch(
    `/${cluster ? 'clusters' : 'instances'}/${id}/kdb/${encodePath(path)}`,
    { credentials: 'same-origin' }
  )
    .then(parseJSONResponse)
    .then(result => {
      return { ...result, id, path }
    }),
})

export const getClusterKey = (id, path, value) => getKey(id, path, true)

// ~~~

export const SET_KEY_REQUEST = 'SET_KEY_REQUEST'
export const SET_KEY_SUCCESS = 'SET_KEY_SUCCESS'
export const SET_KEY_FAILURE = 'SET_KEY_FAILURE'

export const setKey = (id, path, value, cluster = false) => thunkCreator({
  request: { id, path, value }, // TODO: use this syntax everywhere
  types: [SET_KEY_REQUEST, SET_KEY_SUCCESS, SET_KEY_FAILURE],
  promise: fetch(
    `/${cluster ? 'clusters' : 'instances'}/${id}/kdb/${encodePath(path)}`,
    {
      credentials: 'same-origin',
      method: 'PUT',
      headers: {
        'Content-Type': 'text/plain',
      },
      body: value,
    }
  ).then(parseJSONResponse),
})

export const setClusterKey = (id, path, value) => setKey(id, path, value, true)

// ~~~

export const DELETE_KEY_REQUEST = 'DELETE_KEY_REQUEST'
export const DELETE_KEY_SUCCESS = 'DELETE_KEY_SUCCESS'
export const DELETE_KEY_FAILURE = 'DELETE_KEY_FAILURE'

export const deleteKey = (id, path, cluster = false) => thunkCreator({
  request: { id, path },
  types: [DELETE_KEY_REQUEST, DELETE_KEY_SUCCESS, DELETE_KEY_FAILURE],
  promise: fetch(
    `/${cluster ? 'clusters' : 'instances'}/${id}/kdb/${encodePath(path)}`,
    {
      credentials: 'same-origin',
      method: 'DELETE',
    }
  ).then(parseJSONResponse),
})

export const deleteClusterKey = (id, path) => deleteKey(id, path, true)
