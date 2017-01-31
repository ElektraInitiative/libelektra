/**
 * @file
 *
 * @brief the main entry point for routes
 *
 * this exports a function that gets passed the express app as an argument,
 * which can be used to define routes
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

import { successResponse, errorResponse } from './utils'

import getVersions from '../versions'

import initInstanceRoutes from './instances'
import initClusterRoutes from './clusters'

export default function initRoutes (app) {
  app.get('/version', (req, res) =>
    getVersions()
      .then(output => successResponse(res, output))
      .catch(err => errorResponse(res, err))
  )

  initInstanceRoutes(app)
  initClusterRoutes(app)
}
