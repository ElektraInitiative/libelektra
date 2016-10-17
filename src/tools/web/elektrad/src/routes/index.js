import { successResponse, errorResponse } from './utils'

import kdb from '../../../kdb'
import getVersions from '../versions'

export default function initRoutes (app) {
  app.get('/version', (req, res) =>
    getVersions()
      .then(output => successResponse(res, output))
      .catch(err => errorResponse(res, err))
  )

  app.get('/kdb', (req, res) =>
    kdb.getAndLs('/')
      .then(output => successResponse(res, output))
      .catch(err => errorResponse(res, err))
  )

  app.route('/kdb/*')
    .get((req, res) =>
      kdb.getAndLs(req.params[0])
        .then(output => successResponse(res, output))
        .catch(err => errorResponse(res, err))
    )
    .put((req, res) =>
      kdb.set(req.params[0], req.body)
        .then(output => successResponse(res, output))
        .catch(err => errorResponse(res, err))
    )
    .delete((req, res) =>
      kdb.rm(req.params[0])
        .then(output => successResponse(res, output))
        .catch(err => errorResponse(res, err))
    )
}
