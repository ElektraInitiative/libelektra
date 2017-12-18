/**
 * @file
 *
 * @brief this combines all reducers into one tree
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { combineReducers } from 'redux'

import { reducer as idleReducer } from 'redux-promises'

import instancesReducer from './instances'
import containerReducer from './container'
import keyReducer from './kdb'
import pathReducer from './ls'
import errorReducer from './error'

export default combineReducers({
  idle: idleReducer,
  instances: instancesReducer,
  container: containerReducer,
  kdb: keyReducer,
  ls: pathReducer,
  error: errorReducer,
})
