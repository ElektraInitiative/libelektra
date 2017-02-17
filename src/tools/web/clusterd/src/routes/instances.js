/**
 * @file
 *
 * @brief this exports a function that defines routes for the express app
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

import { successResponse, errorResponse } from './utils'

import {
  getInstances, createInstance,
  getInstance, updateInstance, deleteInstance,
} from '../db'

import remoteKdb from '../connector'

export default function initInstanceRoutes (app) {
  app.route('/instances')
    .get((req, res) =>
      getInstances()
        .then(output => successResponse(res, output))
        .catch(err => errorResponse(res, err))
    )
    .post((req, res) =>
      createInstance(req.body)
        .then(output => successResponse(res, output))
        .catch(err => errorResponse(res, err))
    )

  app.route('/instances/:id')
    .get((req, res) =>
      getInstance(req.params.id)
        .then(output => successResponse(res, output))
        .catch(err => errorResponse(res, err))
    )
    .put((req, res) =>
      updateInstance(req.params.id, req.body)
        .then(output => successResponse(res, output))
        .catch(err => errorResponse(res, err))
    )
    .delete((req, res) =>
      deleteInstance(req.params.id)
        .then(output => successResponse(res, output))
        .catch(err => errorResponse(res, err))
    )

  app.get('/instances/:id/version', (req, res) =>
    getInstance(req.params.id)
      .then(instance => remoteKdb.version(instance.host))
      .then(output => successResponse(res, output))
      .catch(err => errorResponse(res, err))
  )

  app.get('/instances/:id/kdb', (req, res) =>
    getInstance(req.params.id)
      .then(instance => remoteKdb.get(instance.host))
      .then(output => successResponse(res, output))
      .catch(err => errorResponse(res, err))
  )

  app.route('/instances/:id/kdb/*')
    .get((req, res) =>
      getInstance(req.params.id)
        .then(instance => remoteKdb.get(instance.host, req.params[0]))
        .then(output => successResponse(res, output))
        .catch(err => errorResponse(res, err))
    )
    .put((req, res) =>
      getInstance(req.params.id)
        .then(instance => remoteKdb.set(instance.host, req.params[0], req.body))
        .then(output => successResponse(res, output))
        .catch(err => errorResponse(res, err))
    )
    .delete((req, res) =>
      getInstance(req.params.id)
        .then(instance => remoteKdb.rm(instance.host, req.params[0]))
        .then(output => successResponse(res, output))
        .catch(err => errorResponse(res, err))
    )
}
