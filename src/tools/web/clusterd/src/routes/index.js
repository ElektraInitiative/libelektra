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
