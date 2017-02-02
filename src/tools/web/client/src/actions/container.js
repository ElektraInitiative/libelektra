/**
 * @file
 *
 * @brief container (main overview) specific actions
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

export const ADD_INSTANCE = 'ADD_INSTANCE'
export const UNADD_INSTANCE = 'UNADD_INSTANCE'

export const addInstance = () => {
  return { type: ADD_INSTANCE }
}

export const unaddInstance = () => {
  return { type: UNADD_INSTANCE }
}

// ~~~

export const ADD_CLUSTER = 'ADD_CLUSTER'
export const UNADD_CLUSTER = 'UNADD_CLUSTER'

export const addCluster = () => {
  return { type: ADD_CLUSTER }
}

export const unaddCluster = () => {
  return { type: UNADD_CLUSTER }
}

// ~~~

export const SELECT_INSTANCE = 'SELECT_INSTANCE'

export const selectInstance = (instanceId) => {
  return { type: SELECT_INSTANCE, instanceId }
}
