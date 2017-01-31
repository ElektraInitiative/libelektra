/* reducers/error.js
handle errors that happened in actions
they will be shown in a small notification at the bottom of the UI
*/

import {
  INSTANCES_FAILURE, INSTANCE_UPDATE_FAILURE, INSTANCE_DELETE_FAILURE,
  CREATE_INSTANCE_FAILURE,
  CLUSTERS_FAILURE, CLUSTER_UPDATE_FAILURE, CLUSTER_DELETE_FAILURE,
  CREATE_CLUSTER_FAILURE,
  GET_KEY_FAILURE, SET_KEY_FAILURE,
  CONFIGURE_INSTANCE_FAILURE, CONFIGURE_CLUSTER_FAILURE,
} from '../actions'

export default function errorReducer (state = false, action) {
  switch (action.type) {
    case INSTANCES_FAILURE:
    case INSTANCE_UPDATE_FAILURE:
    case INSTANCE_DELETE_FAILURE:
    case CREATE_INSTANCE_FAILURE:
    case CLUSTERS_FAILURE:
    case CLUSTER_UPDATE_FAILURE:
    case CLUSTER_DELETE_FAILURE:
    case CREATE_CLUSTER_FAILURE:
    case GET_KEY_FAILURE:
    case SET_KEY_FAILURE:
      return action.error

    case CONFIGURE_INSTANCE_FAILURE:
    case CONFIGURE_CLUSTER_FAILURE:
      if (action.error && action.error.message &&
        action.error.message === 'only absolute urls are supported'
      ) {
        return { ...action.error, message: 'invalid url specified as host' }
      } else {
        return action.error
      }

    default:
      return state
  }
}
