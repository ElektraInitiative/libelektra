/**
 * @file
 *
 * @brief handle actions related to clusters
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

import {
  CLUSTERS_SUCCESS, CLUSTER_DELETE_SUCCESS, CLUSTER_UPDATE_SUCCESS,
  CREATE_CLUSTER_SUCCESS,
} from '../actions'

export default function clustersReducer (state = [], action) {
  switch (action.type) {
    case CLUSTER_DELETE_SUCCESS: // cluster deleted, remove from state
      return state.filter(
        (cluster) => cluster.id !== action.result.id
      )

    case CLUSTER_UPDATE_SUCCESS: // cluster updated, update in state
      return state.map(
        (cluster) =>
          cluster.id === action.result.id
          ? action.result
          : cluster
      )

    case CLUSTERS_SUCCESS: // cluster list pulled from clusterd, update state
      return action.result

    case CREATE_CLUSTER_SUCCESS: // cluster created, add to state
      return [ ...state, action.result ]

    default:
      return state
  }
}
