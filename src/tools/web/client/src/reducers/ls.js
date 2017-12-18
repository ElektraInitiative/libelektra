/**
 * @file
 *
 * @brief handle actions related to operations on the *shallow*
 * Elektra key database (kdb), which only contains paths
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { GET_KDB_SUCCESS } from '../actions'

// controls the state of the paths available in the kdb
export default function pathReducer (state = {}, action) {
  if (action.type === GET_KDB_SUCCESS) {
    if (action.result && action.result.ls) {
      return action.result.ls
    }
  }
  return state
}
