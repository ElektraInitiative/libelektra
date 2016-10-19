import {
  INSTANCES_SUCCESS, INSTANCE_DELETE_SUCCESS, INSTANCE_UPDATE_SUCCESS,
  CREATE_INSTANCE_SUCCESS,
} from '../actions'

export default function instancesReducer (state = [], action) {
  switch (action.type) {
    case INSTANCE_DELETE_SUCCESS: // instance deleted, remove from state
      return state.filter(
        (instance) => instance.id !== action.result.id
      )

    case INSTANCE_UPDATE_SUCCESS: // instance updated, update in state
      return state.map(
        (instance) =>
          instance.id === action.result.id
          ? action.result
          : instance
      )

    case INSTANCES_SUCCESS: // instance list pulled from clusterd, update state
      return action.result

    case CREATE_INSTANCE_SUCCESS: // instance created, add to state
      return [ ...state, action.result ]

    default:
      return state
  }
}
