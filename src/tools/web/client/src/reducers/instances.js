import {
  INSTANCES_SUCCESS, INSTANCE_DELETE_SUCCESS, INSTANCE_UPDATE_SUCCESS,
  CREATE_INSTANCE_SUCCESS,
} from '../actions'

export default function instancesReducer (state = [], action) {
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
