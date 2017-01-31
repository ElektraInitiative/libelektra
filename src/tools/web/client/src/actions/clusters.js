/* actions/clusters.js
cluster specific actions
*/

import { thunkCreator } from './utils'

// ~~~

export const CLUSTERS_REQUEST = 'CLUSTERS_REQUEST'
export const CLUSTERS_SUCCESS = 'CLUSTERS_SUCCESS'
export const CLUSTERS_FAILURE = 'CLUSTERS_FAILURE'

export const fetchClusters = () => thunkCreator({
  types: [CLUSTERS_REQUEST, CLUSTERS_SUCCESS, CLUSTERS_FAILURE],
  promise: fetch(`/clusters`)
    .then(response => response.json()),
})

// ~~~

export const CLUSTER_UPDATE_REQUEST = 'CLUSTER_UPDATE_REQUEST'
export const CLUSTER_UPDATE_SUCCESS = 'CLUSTER_UPDATE_SUCCESS'
export const CLUSTER_UPDATE_FAILURE = 'CLUSTER_UPDATE_FAILURE'

export const updateCluster = (id, data) => thunkCreator({
  types: [CLUSTER_UPDATE_REQUEST, CLUSTER_UPDATE_SUCCESS, CLUSTER_UPDATE_FAILURE],
  promise: fetch(`/clusters/${id}`, {
    method: 'PUT',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(data),
  }).then(response => response.json()),
})

// ~~~

export const CLUSTER_DELETE_REQUEST = 'CLUSTER_DELETE_REQUEST'
export const CLUSTER_DELETE_SUCCESS = 'CLUSTER_DELETE_SUCCESS'
export const CLUSTER_DELETE_FAILURE = 'CLUSTER_DELETE_FAILURE'

export const deleteCluster = (id, data) => thunkCreator({
  types: [CLUSTER_DELETE_REQUEST, CLUSTER_DELETE_SUCCESS, CLUSTER_DELETE_FAILURE],
  promise: fetch(`/clusters/${id}`, { method: 'DELETE' })
    .then(response => response.json()),
})

// ~~~

export const CREATE_CLUSTER_REQUEST = 'CREATE_CLUSTER_REQUEST'
export const CREATE_CLUSTER_SUCCESS = 'CREATE_CLUSTER_SUCCESS'
export const CREATE_CLUSTER_FAILURE = 'CREATE_CLUSTER_FAILURE'

export const createCluster = (data) => thunkCreator({
  types: [CREATE_CLUSTER_REQUEST, CREATE_CLUSTER_SUCCESS, CREATE_CLUSTER_FAILURE],
  promise: fetch(`/clusters`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(data),
  }).then(response => response.json()),
})
