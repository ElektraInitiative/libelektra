/**
 * @file
 *
 * @brief instance specific actions
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

import { thunkCreator } from './utils'

// ~~~

export const INSTANCES_REQUEST = 'INSTANCES_REQUEST'
export const INSTANCES_SUCCESS = 'INSTANCES_SUCCESS'
export const INSTANCES_FAILURE = 'INSTANCES_FAILURE'

export const fetchInstances = () => thunkCreator({
  types: [INSTANCES_REQUEST, INSTANCES_SUCCESS, INSTANCES_FAILURE],
  promise: fetch(`/instances`)
    .then(response => response.json()),
})

// ~~~

export const INSTANCE_UPDATE_REQUEST = 'INSTANCE_UPDATE_REQUEST'
export const INSTANCE_UPDATE_SUCCESS = 'INSTANCE_UPDATE_SUCCESS'
export const INSTANCE_UPDATE_FAILURE = 'INSTANCE_UPDATE_FAILURE'

export const updateInstance = (id, data) => thunkCreator({
  types: [INSTANCE_UPDATE_REQUEST, INSTANCE_UPDATE_SUCCESS, INSTANCE_UPDATE_FAILURE],
  promise: fetch(`/instances/${id}`, {
    method: 'PUT',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(data),
  }).then(response => response.json()),
})

// ~~~

export const INSTANCE_DELETE_REQUEST = 'INSTANCE_DELETE_REQUEST'
export const INSTANCE_DELETE_SUCCESS = 'INSTANCE_DELETE_SUCCESS'
export const INSTANCE_DELETE_FAILURE = 'INSTANCE_DELETE_FAILURE'

export const deleteInstance = (id, data) => thunkCreator({
  types: [INSTANCE_DELETE_REQUEST, INSTANCE_DELETE_SUCCESS, INSTANCE_DELETE_FAILURE],
  promise: fetch(`/instances/${id}`, { method: 'DELETE' })
    .then(response => response.json()),
})

// ~~~

export const CREATE_INSTANCE_REQUEST = 'CREATE_INSTANCE_REQUEST'
export const CREATE_INSTANCE_SUCCESS = 'CREATE_INSTANCE_SUCCESS'
export const CREATE_INSTANCE_FAILURE = 'CREATE_INSTANCE_FAILURE'

export const createInstance = (data) => thunkCreator({
  types: [CREATE_INSTANCE_REQUEST, CREATE_INSTANCE_SUCCESS, CREATE_INSTANCE_FAILURE],
  promise: fetch(`/instances`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(data),
  }).then(response => response.json()),
})
