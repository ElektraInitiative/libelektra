import {
  ADD_INSTANCE, UNADD_INSTANCE, ADD_CLUSTER, UNADD_CLUSTER,
  CREATE_INSTANCE_SUCCESS, CREATE_CLUSTER_SUCCESS,
  SELECT_INSTANCE,
} from '../actions'

export default function containerReducer (
  state = { addingInstance: false, addingCluster: false, clusterInstances: [] },
  action
) {
  switch (action.type) {
    case ADD_INSTANCE:
      return { ...state, addingInstance: true }
    case UNADD_INSTANCE:
    case CREATE_INSTANCE_SUCCESS:
      return { ...state, addingInstance: false }
    case CREATE_CLUSTER_SUCCESS:
      return { ...state, addingCluster: false, clusterInstances: [] }
    case ADD_CLUSTER:
      return { ...state, addingCluster: true, clusterInstances: [] }
    case UNADD_CLUSTER:
      return { ...state, addingCluster: false }
    case SELECT_INSTANCE:
      return { ...state,
        clusterInstances: [ ...state.clusterInstances, action.instanceId ],
      }
    default:
      return state
  }
}
