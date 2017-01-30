import {
  CONFIGURE_INSTANCE_SUCCESS, CONFIGURE_CLUSTER_SUCCESS, RETURN_TO_MAIN,
  DELETE_KEY_REQUEST,
} from './actions'

export const PAGE_MAIN = 'PAGE_MAIN'
export const PAGE_CONFIGURE = 'PAGE_CONFIGURE'

// simple router that switches pages on certain actions
export default function routerReducer (
  state = { page: PAGE_MAIN },
  action
) {
  switch (action.type) {
    case CONFIGURE_CLUSTER_SUCCESS:
      return { ...action.result, page: PAGE_CONFIGURE, configuring: 'cluster' }
    case CONFIGURE_INSTANCE_SUCCESS:
      return { ...action.result, page: PAGE_CONFIGURE, configuring: 'instance' }
    case RETURN_TO_MAIN:
      return { page: PAGE_MAIN }
    case DELETE_KEY_REQUEST:
      return {
        ...state,
        ls: state.ls.filter(path => path !== action.request.path),
      }
    default:
      return state
  }
}
