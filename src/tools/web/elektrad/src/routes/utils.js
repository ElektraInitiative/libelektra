/**
 * @file
 *
 * @brief utility functions used in routes
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

export const prettyprint = (obj) =>
  JSON.stringify(obj, null, 2)

export const successResponse = (res, output) =>
  output ? res.send(prettyprint(output)) : res.status(404).send()

export const errorResponse = (res, err) =>
  res.status(400).send(prettyprint({ error: err.message }))
