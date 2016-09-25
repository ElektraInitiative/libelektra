import { combineReducers } from 'redux'
import { reducer as idleReducer } from 'redux-promises'
import { routerReducer } from './router'

import {
  INSTANCES_SUCCESS, INSTANCE_DELETE_SUCCESS, INSTANCE_UPDATE_SUCCESS,
  CREATE_INSTANCE_SUCCESS,
  ADD_INSTANCE, UNADD_INSTANCE, ADD_CLUSTER, UNADD_CLUSTER,
  GET_KEY_SUCCESS, SET_KEY_REQUEST,
} from './actions'

const instancesReducer = (state = [], action) => {
  switch (action.type) {
    // TODO: handle _REQUESTED and _FAILURE

    case INSTANCE_DELETE_SUCCESS:
      return state.filter(
        (instance) => instance.id !== action.result.id
      )

    case INSTANCE_UPDATE_SUCCESS:
      return state.map(
        (instance) =>
          instance.id === action.result.id
          ? action.result
          : instance
      )

    case INSTANCES_SUCCESS:
      return action.result

    case CREATE_INSTANCE_SUCCESS:
      return [ ...state, action.result ]

    default:
      return state
  }
}

const containerReducer = (
  state = { addingInstance: false, addingCluster: false },
  action) => {
    switch (action.type) {
      case ADD_INSTANCE:
        return { ...state, addingInstance: true }
      case UNADD_INSTANCE:
      case CREATE_INSTANCE_SUCCESS:
        return { ...state, addingInstance: false }
      case ADD_CLUSTER:
        return { ...state, addingCluster: true }
      case UNADD_CLUSTER:
        return { ...state, addingCluster: false }
      default:
        return state
    }
}

const updateState = (state, { id, path, value }) => {
  return {
    ...state,
    [id]: {
      ...state[id],
      [path]: value,
    },
  }
}

const keyReducer = (state = {}, action) => {
  switch (action.type) {
    case GET_KEY_SUCCESS:
      return updateState(state, action.result)
    case SET_KEY_REQUEST: // TODO: handle failure
      return updateState(state, action.request)
    default:
      return state
  }
}

const rootReducer = combineReducers({
  idle: idleReducer,
  instances: instancesReducer,
  container: containerReducer,
  router: routerReducer,
  kdb: keyReducer,
})
export default rootReducer
