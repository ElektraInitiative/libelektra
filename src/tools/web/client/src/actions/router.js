/**
 * @file
 *
 * @brief router specific actions
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { thunkCreator, parseJSONResponse } from './utils'

// ~~~

export const CONFIGURE_INSTANCE_REQUEST = 'CONFIGURE_INSTANCE_REQUEST'
export const CONFIGURE_INSTANCE_SUCCESS = 'CONFIGURE_INSTANCE_SUCCESS'
export const CONFIGURE_INSTANCE_FAILURE = 'CONFIGURE_INSTANCE_FAILURE'

export const configureInstance = (id) => thunkCreator({
  id,
  types: [CONFIGURE_INSTANCE_REQUEST, CONFIGURE_INSTANCE_SUCCESS, CONFIGURE_INSTANCE_FAILURE],
  promise: fetch(`/instances/${id}/kdb`, { credentials: 'same-origin' })
    .then(parseJSONResponse)
    .then(result => {
      return { ...result, id }
    }),
})

// ~~~

export const RETURN_TO_MAIN = 'RETURN_TO_MAIN'

export const returnToMain = () => {
  return { type: RETURN_TO_MAIN }
}
