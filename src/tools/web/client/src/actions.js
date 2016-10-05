import { thunkCreator, encodePath } from './utils'

// TODO: don't hardcode host
const HOST = 'http://127.0.0.1:1235'

//~~~

export const INSTANCES_REQUEST = 'INSTANCES_REQUEST'
export const INSTANCES_SUCCESS = 'INSTANCES_SUCCESS'
export const INSTANCES_FAILURE = 'INSTANCES_FAILURE'

export const fetchInstances = () => thunkCreator({
  types: [INSTANCES_REQUEST, INSTANCES_SUCCESS, INSTANCES_FAILURE],
  promise: fetch(`${HOST}/instances`)
    .then(response => response.json()),
})

//~~~

export const INSTANCE_UPDATE_REQUEST = 'INSTANCE_UPDATE_REQUEST'
export const INSTANCE_UPDATE_SUCCESS = 'INSTANCE_UPDATE_SUCCESS'
export const INSTANCE_UPDATE_FAILURE = 'INSTANCE_UPDATE_FAILURE'

export const updateInstance = (id, data) => thunkCreator({
  types: [INSTANCE_UPDATE_REQUEST, INSTANCE_UPDATE_SUCCESS, INSTANCE_UPDATE_FAILURE],
  promise: fetch(`${HOST}/instances/${id}`, {
    method: 'PUT',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(data),
  }).then(response => response.json()),
})

//~~~

export const INSTANCE_DELETE_REQUEST = 'INSTANCE_DELETE_REQUEST'
export const INSTANCE_DELETE_SUCCESS = 'INSTANCE_DELETE_SUCCESS'
export const INSTANCE_DELETE_FAILURE = 'INSTANCE_DELETE_FAILURE'

export const deleteInstance = (id, data) => thunkCreator({
  types: [INSTANCE_DELETE_REQUEST, INSTANCE_DELETE_SUCCESS, INSTANCE_DELETE_FAILURE],
  promise: fetch(`${HOST}/instances/${id}`, { method: 'DELETE' })
    .then(response => response.json()),
})

//~~~

export const CLUSTERS_REQUEST = 'CLUSTERS_REQUEST'
export const CLUSTERS_SUCCESS = 'CLUSTERS_SUCCESS'
export const CLUSTERS_FAILURE = 'CLUSTERS_FAILURE'

export const fetchClusters = () => thunkCreator({
  types: [CLUSTERS_REQUEST, CLUSTERS_SUCCESS, CLUSTERS_FAILURE],
  promise: fetch(`${HOST}/clusters`)
    .then(response => response.json()),
})

//~~~

export const CLUSTER_UPDATE_REQUEST = 'CLUSTER_UPDATE_REQUEST'
export const CLUSTER_UPDATE_SUCCESS = 'CLUSTER_UPDATE_SUCCESS'
export const CLUSTER_UPDATE_FAILURE = 'CLUSTER_UPDATE_FAILURE'

export const updateCluster = (id, data) => thunkCreator({
  types: [CLUSTER_UPDATE_REQUEST, CLUSTER_UPDATE_SUCCESS, CLUSTER_UPDATE_FAILURE],
  promise: fetch(`${HOST}/clusters/${id}`, {
    method: 'PUT',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(data),
  }).then(response => response.json()),
})

//~~~

export const CLUSTER_DELETE_REQUEST = 'CLUSTER_DELETE_REQUEST'
export const CLUSTER_DELETE_SUCCESS = 'CLUSTER_DELETE_SUCCESS'
export const CLUSTER_DELETE_FAILURE = 'CLUSTER_DELETE_FAILURE'

export const deleteCluster = (id, data) => thunkCreator({
  types: [CLUSTER_DELETE_REQUEST, CLUSTER_DELETE_SUCCESS, CLUSTER_DELETE_FAILURE],
  promise: fetch(`${HOST}/clusters/${id}`, { method: 'DELETE' })
    .then(response => response.json()),
})

//~~~

export const ADD_INSTANCE = 'ADD_INSTANCE'
export const UNADD_INSTANCE = 'UNADD_INSTANCE'

export const addInstance = () => {
  return { type: ADD_INSTANCE }
}

export const unaddInstance = () => {
  return { type: UNADD_INSTANCE }
}

export const ADD_CLUSTER = 'ADD_CLUSTER'
export const UNADD_CLUSTER = 'UNADD_CLUSTER'

export const addCluster = () => {
  return { type: ADD_CLUSTER }
}

export const unaddCluster = () => {
  return { type: UNADD_CLUSTER }
}

export const SELECT_INSTANCE = 'SELECT_INSTANCE'

export const selectInstance = (instanceId) => {
  return { type: SELECT_INSTANCE, instanceId }
}

//~~~

export const CONFIGURE_INSTANCE_REQUEST = 'CONFIGURE_INSTANCE_REQUEST'
export const CONFIGURE_INSTANCE_SUCCESS = 'CONFIGURE_INSTANCE_SUCCESS'
export const CONFIGURE_INSTANCE_FAILURE = 'CONFIGURE_INSTANCE_FAILURE'

export const configureInstance = (id) => thunkCreator({
  types: [CONFIGURE_INSTANCE_REQUEST, CONFIGURE_INSTANCE_SUCCESS, CONFIGURE_INSTANCE_FAILURE],
  promise: fetch(`${HOST}/instances/${id}/kdb`)
    .then(response => response.json())
    .then(result => {
      return { ...result, id }
    }),
})

//~~~

export const CONFIGURE_CLUSTER_REQUEST = 'CONFIGURE_CLUSTER_REQUEST'
export const CONFIGURE_CLUSTER_SUCCESS = 'CONFIGURE_CLUSTER_SUCCESS'
export const CONFIGURE_CLUSTER_FAILURE = 'CONFIGURE_CLUSTER_FAILURE'

export const configureCluster = (id) => thunkCreator({
  types: [CONFIGURE_CLUSTER_REQUEST, CONFIGURE_CLUSTER_SUCCESS, CONFIGURE_CLUSTER_FAILURE],
  promise: fetch(`${HOST}/clusters/${id}/kdb`)
    .then(response => response.json())
    .then(result => {
      return { ...result, id }
    }),
})

//~~~

export const GET_KEY_REQUEST = 'GET_KEY_REQUEST'
export const GET_KEY_SUCCESS = 'GET_KEY_SUCCESS'
export const GET_KEY_FAILURE = 'GET_KEY_FAILURE'

export const getKey = (id, path) => thunkCreator({
  types: [GET_KEY_REQUEST, GET_KEY_SUCCESS, GET_KEY_FAILURE],
  promise: fetch(`${HOST}/instances/${id}/kdb/${encodePath(path)}`)
    .then(response => response.json())
    .then(result => {
      return { ...result, id, path }
    }),
})

export const SET_KEY_REQUEST = 'SET_KEY_REQUEST'
export const SET_KEY_SUCCESS = 'SET_KEY_SUCCESS'
export const SET_KEY_FAILURE = 'SET_KEY_FAILURE'

export const setKey = (id, path, value) => thunkCreator({
  request: { id, path, value }, // TODO: use this syntax everywhere
  types: [SET_KEY_REQUEST, SET_KEY_SUCCESS, SET_KEY_FAILURE],
  promise: fetch(`${HOST}/instances/${id}/kdb/${encodePath(path)}`, {
    method: 'PUT',
    headers: {
      'Content-Type': 'text/plain',
    },
    body: value,
  }).then(response => response.json()),
})

export const RETURN_TO_MAIN = 'RETURN_TO_MAIN'

export const returnToMain = () => {
  return { type: RETURN_TO_MAIN }
}

//~~~

export const CREATE_INSTANCE_REQUEST = 'CREATE_INSTANCE_REQUEST'
export const CREATE_INSTANCE_SUCCESS = 'CREATE_INSTANCE_SUCCESS'
export const CREATE_INSTANCE_FAILURE = 'CREATE_INSTANCE_FAILURE'

export const createInstance = (data) => thunkCreator({
  types: [CREATE_INSTANCE_REQUEST, CREATE_INSTANCE_SUCCESS, CREATE_INSTANCE_FAILURE],
  promise: fetch(`${HOST}/instances`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(data),
  }).then(response => response.json()),
})

//~~~

export const CREATE_CLUSTER_REQUEST = 'CREATE_CLUSTER_REQUEST'
export const CREATE_CLUSTER_SUCCESS = 'CREATE_CLUSTER_SUCCESS'
export const CREATE_CLUSTER_FAILURE = 'CREATE_CLUSTER_FAILURE'

export const createCluster = (data) => thunkCreator({
  types: [CREATE_CLUSTER_REQUEST, CREATE_CLUSTER_SUCCESS, CREATE_CLUSTER_FAILURE],
  promise: fetch(`${HOST}/clusters`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
    },
    body: JSON.stringify(data),
  }).then(response => response.json()),
})
