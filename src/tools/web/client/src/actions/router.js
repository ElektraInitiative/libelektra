import { thunkCreator } from './utils'

//~~~

export const CONFIGURE_INSTANCE_REQUEST = 'CONFIGURE_INSTANCE_REQUEST'
export const CONFIGURE_INSTANCE_SUCCESS = 'CONFIGURE_INSTANCE_SUCCESS'
export const CONFIGURE_INSTANCE_FAILURE = 'CONFIGURE_INSTANCE_FAILURE'

export const configureInstance = (id) => thunkCreator({
  types: [CONFIGURE_INSTANCE_REQUEST, CONFIGURE_INSTANCE_SUCCESS, CONFIGURE_INSTANCE_FAILURE],
  promise: fetch(`/instances/${id}/kdb`)
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
  promise: fetch(`/clusters/${id}/kdb`)
    .then(response => response.json())
    .then(result => {
      return { ...result, id }
    }),
})

//~~~

export const RETURN_TO_MAIN = 'RETURN_TO_MAIN'

export const returnToMain = () => {
  return { type: RETURN_TO_MAIN }
}
