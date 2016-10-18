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

export default function keyReducer (state = {}, action) {
  switch (action.type) {
    case GET_KEY_SUCCESS:
      return updateState(state, action.result)
    case SET_KEY_REQUEST: // TODO: handle failure
      return updateState(state, action.request)
    default:
      return state
  }
}
