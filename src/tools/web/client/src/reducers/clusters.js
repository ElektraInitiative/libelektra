import {
  CLUSTERS_SUCCESS, CLUSTER_DELETE_SUCCESS, CLUSTER_UPDATE_SUCCESS,
  CREATE_CLUSTER_SUCCESS,
} from '../actions'

export default function clustersReducer (state = [], action) {
  switch (action.type) {
    // TODO: handle _REQUESTED and _FAILURE

    case CLUSTER_DELETE_SUCCESS:
      return state.filter(
        (cluster) => cluster.id !== action.result.id
      )

    case CLUSTER_UPDATE_SUCCESS:
      return state.map(
        (cluster) =>
          cluster.id === action.result.id
          ? action.result
          : instance
      )

    case CLUSTERS_SUCCESS:
      return action.result

    case CREATE_CLUSTER_SUCCESS:
      return [ ...state, action.result ]

    default:
      return state
  }
}
