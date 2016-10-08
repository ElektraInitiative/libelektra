import parallel from 'async/parallel'
import { successResponse, errorResponse } from './utils'

import kdb from '../../../kdb'
import VERSIONS from '../versions'

// TODO: handle errors

const kdbGet = (path) =>
  Promise.all(
    [ kdb.ls(path), kdb.get(path) ] // execute ls and get in parallel
  ).then(([ ls, value ]) => {
    return { ls, value } // return results as object
  })

export default function initRoutes (app) {
  app.get('/version', (req, res) =>
    successResponse(res, VERSIONS)
  )

  app.get('/kdb', (req, res) =>
    kdbGet('/')
      .then(output => successResponse(res, output))
      .catch(err => errorResponse(res, err))
  )

  app.route('/kdb/*')
    .get((req, res) =>
      kdbGet(req.params[0])
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
