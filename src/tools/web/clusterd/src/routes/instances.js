/**
 * @file
 *
 * @brief this exports a function that defines routes for the express app
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { APIError, successResponse, errorResponse, dontShowDB } from './utils'

import {
  getInstances as getDBInstances, createInstance,
  getInstance as getDBInstance, updateInstance, deleteInstance,
} from '../db'

import { getSingleInstance } from '../config'

import remoteKdb from '../connector'

const makeMyInstance = (host) => {
  if (!host) return false
  return { host, id: 'my', name: 'My' }
}

const getInstance = (id) =>
  (id === 'my')
    ? getSingleInstance().then(makeMyInstance)
    : getDBInstance(id)

const getInstances = () =>
  getSingleInstance()
    .then(host => {
      if (!host) return getDBInstances()
      return getDBInstances()
        .then(instances => instances.concat([ makeMyInstance(host) ]))
    })

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
      .then(instance => {
        if (!instance || !instance.host) {
          throw new APIError('Instance not found or invalid (no host)')
        }
        return remoteKdb.get(instance.host)
      })
      .then(dontShowDB)
      .then(output => successResponse(res, output))
      .catch(err => errorResponse(res, err))
  )

  app.route('/instances/:id/kdb/*')
    .get((req, res) =>
      getInstance(req.params.id)
        .then(instance => remoteKdb.get(instance.host, req.params[0]))
        .then(dontShowDB)
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

  app.post('/instances/:id/kdbMv/*', (req, res) =>
    getInstance(req.params.id)
      .then(instance => remoteKdb.mv(instance.host, req.params[0], req.body))
      .then(() => res.status(204).send())
      .catch(err => errorResponse(res, err))
  )

  app.post('/instances/:id/kdbCp/*', (req, res) =>
    getInstance(req.params.id)
      .then(instance => remoteKdb.cp(instance.host, req.params[0], req.body))
      .then(() => res.status(204).send())
      .catch(err => errorResponse(res, err))
  )

  app.route('/instances/:id/kdbMeta/*')
    .post((req, res) =>
      getInstance(req.params.id)
        .then(instance => remoteKdb.setmeta(instance.host, req.params[0], req.body.key, req.body.value))
        .then(() => res.status(204).send())
        .catch(err => errorResponse(res, err))
    )
    .delete((req, res) =>
      getInstance(req.params.id)
        .then(instance => remoteKdb.rmmeta(instance.host, req.params[0], req.body.key))
        .then(() => res.status(204).send())
        .catch(err => errorResponse(res, err))
    )
}
