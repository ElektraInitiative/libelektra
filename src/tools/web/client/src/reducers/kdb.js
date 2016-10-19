import { GET_KEY_SUCCESS, SET_KEY_REQUEST } from '../actions'

const updateState = (state, { id, path, value }) => {
  return {
    ...state,
    [id]: {
      ...state[id],
      [path]: value,
    },
  }
}

// controls the state of the local kdb cache
export default function keyReducer (state = {}, action) {
  switch (action.type) {
    case GET_KEY_SUCCESS:
      return updateState(state, action.result)
    case SET_KEY_REQUEST:
      return updateState(state, action.request)
    // TODO: specifically handle failure (show error inline)
    default:
      return state
  }
}
