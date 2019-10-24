/**
 * @file
 *
 * @brief utility functions used in routes
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

export const prettyprint = obj => JSON.stringify(obj, null, 2);

export const successResponse = (res, output) =>
  output ? res.json(output) : res.send();

export const errorResponse = (res, err) => res.status(400).json({ error: err });
