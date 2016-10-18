import { combineReducers } from 'redux'

import { reducer as idleReducer } from 'redux-promises'
import routerReducer from '../router'

import clustersReducer from './clusters'
import instancesReducer from './instances'
import containerReducer from './container'
import keyReducer from './kdb'

export default combineReducers({
  idle: idleReducer,
  router: routerReducer,
  clusters: clustersReducer,
  instances: instancesReducer,
  container: containerReducer,
  kdb: keyReducer,
})
