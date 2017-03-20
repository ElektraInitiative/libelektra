/**
 * @file
 *
 * @brief utility functions used in routes
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

import { ROOT_PATH } from '../config'

export const prettyprint = (obj) =>
  JSON.stringify(obj, null, 2)

export const successResponse = (res, output) =>
  output
    ? res.type('application/json').send(prettyprint(output))
    : res.status(404).send() // no output -> 404

export const errorResponse = (res, err) => {
  if (process.env.NODE_ENV !== 'production') console.error(err)
  const errObj = (err instanceof Error)
    ? { name: err.name, message: err.message }
    : err
  return res.status(400).type('application/json')
            .send(prettyprint({ error: errObj }))
}

// don't show the internal database via the API
export const dontShowDB = (output) => {
  if (output && Array.isArray(output.ls)) {
    output.ls = output.ls.filter(
      path => !path.startsWith(ROOT_PATH)
    )
  }
  return output
}
