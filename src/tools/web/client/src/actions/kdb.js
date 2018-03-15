/**
 * @file
 *
 * @brief Elektra key database (kdb) specific actions, used in the tree view
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { thunkCreator, encodePath, parseJSONResponse } from './utils'

// ~~~

export const GET_KDB_REQUEST = 'GET_KDB_REQUEST'
export const GET_KDB_SUCCESS = 'GET_KDB_SUCCESS'
export const GET_KDB_FAILURE = 'GET_KDB_FAILURE'

export const getKdb = (id) => thunkCreator({
  id,
  types: [GET_KDB_REQUEST, GET_KDB_SUCCESS, GET_KDB_FAILURE],
  promise: fetch(`/api/instances/${id}/kdb`, { credentials: 'same-origin' })
    .then(parseJSONResponse)
    .then(result => {
      return { ...result, id }
    }),
})

// ~~~

export const GET_KEY_REQUEST = 'GET_KEY_REQUEST'
export const GET_KEY_SUCCESS = 'GET_KEY_SUCCESS'
export const GET_KEY_FAILURE = 'GET_KEY_FAILURE'

export const getKey = (id, path) => thunkCreator({
  id, path,
  types: [GET_KEY_REQUEST, GET_KEY_SUCCESS, GET_KEY_FAILURE],
  promise: fetch(
    `/api/instances/${id}/kdb/${encodePath(path)}`,
    { credentials: 'same-origin' }
  )
    .then(parseJSONResponse)
    .then(result => {
      return { ...result, id, path }
    }),
})

// ~~~

export const SET_KEY_REQUEST = 'SET_KEY_REQUEST'
export const SET_KEY_SUCCESS = 'SET_KEY_SUCCESS'
export const SET_KEY_FAILURE = 'SET_KEY_FAILURE'

export const setKey = (id, path, value) => thunkCreator({
  id, path, value,
  request: { id, path, value }, // TODO: use this syntax everywhere
  types: [SET_KEY_REQUEST, SET_KEY_SUCCESS, SET_KEY_FAILURE],
  promise: fetch(
    `/api/instances/${id}/kdb/${encodePath(path)}`,
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

// ~~~

export const DELETE_KEY_REQUEST = 'DELETE_KEY_REQUEST'
export const DELETE_KEY_SUCCESS = 'DELETE_KEY_SUCCESS'
export const DELETE_KEY_FAILURE = 'DELETE_KEY_FAILURE'

export const deleteKey = (id, path) => thunkCreator({
  request: { id, path },
  types: [DELETE_KEY_REQUEST, DELETE_KEY_SUCCESS, DELETE_KEY_FAILURE],
  promise: fetch(
    `/api/instances/${id}/kdb/${encodePath(path)}`,
    {
      credentials: 'same-origin',
      method: 'DELETE',
    }
  ).then(parseJSONResponse),
})

// ~~~

export const MOVE_KEY_REQUEST = 'MOVE_KEY_REQUEST'
export const MOVE_KEY_SUCCESS = 'MOVE_KEY_SUCCESS'
export const MOVE_KEY_FAILURE = 'MOVE_KEY_FAILURE'

export const moveKey = (id, from, to) => thunkCreator({
  request: { id, from, to },
  types: [MOVE_KEY_REQUEST, MOVE_KEY_SUCCESS, MOVE_KEY_FAILURE],
  promise: fetch(
    `/api/instances/${id}/kdbMv/${encodePath(from)}`,
    {
      credentials: 'same-origin',
      method: 'POST',
      headers: {
        'Content-Type': 'text/plain',
      },
      body: to,
    }
  ),
})

// ~~~

export const COPY_KEY_REQUEST = 'COPY_KEY_REQUEST'
export const COPY_KEY_SUCCESS = 'COPY_KEY_SUCCESS'
export const COPY_KEY_FAILURE = 'COPY_KEY_FAILURE'

export const copyKey = (id, from, to) => thunkCreator({
  request: { id, from, to },
  types: [COPY_KEY_REQUEST, COPY_KEY_SUCCESS, COPY_KEY_FAILURE],
  promise: fetch(
    `/api/instances/${id}/kdbCp/${encodePath(from)}`,
    {
      credentials: 'same-origin',
      method: 'POST',
      headers: {
        'Content-Type': 'text/plain',
      },
      body: to,
    }
  ),
})

// ~~~

export const SET_META_REQUEST = 'SET_META_REQUEST'
export const SET_META_SUCCESS = 'SET_META_SUCCESS'
export const SET_META_FAILURE = 'SET_META_FAILURE'

export const setMetaKey = (id, path, key, value) => thunkCreator({
  request: { id, path, key, value },
  types: [SET_META_REQUEST, SET_META_SUCCESS, SET_META_FAILURE],
  promise: fetch(
    `/api/instances/${id}/kdbMeta/${encodePath(path)}`,
    {
      credentials: 'same-origin',
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ key, value }),
    }
  ),
})

// ~~~

export const DELETE_META_REQUEST = 'DELETE_META_REQUEST'
export const DELETE_META_SUCCESS = 'DELETE_META_SUCCESS'
export const DELETE_META_FAILURE = 'DELETE_META_FAILURE'

export const deleteMetaKey = (id, path, key) => thunkCreator({
  request: { id, path, key },
  types: [DELETE_META_REQUEST, DELETE_META_SUCCESS, DELETE_META_FAILURE],
  promise: fetch(
    `/api/instances/${id}/kdbMeta/${encodePath(path)}`,
    {
      credentials: 'same-origin',
      method: 'DELETE',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify({ key }),
    }
  ),
})
