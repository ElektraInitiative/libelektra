/**
 * @file
 *
 * @brief this exports a function that defines routes for the express app
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

import { successResponse, errorResponse, dontShowDB } from './utils'

import {
  getInstances as getDBInstances, createInstance,
  getInstance as getDBInstance, updateInstance, deleteInstance,
} from '../db'

import { INSTANCE } from '../config'

import remoteKdb from '../connector'

const myInstance = () => {
  return { host: INSTANCE, id: 'my', name: 'My Instance' }
}

const getInstance = (id) =>
  (INSTANCE && id === 'my')
    ? Promise.resolve(myInstance())
    : getDBInstance(id)

const getInstances = () => {
  if (INSTANCE) {
    return getDBInstances()
      .then(instances => instances.concat([ myInstance() ]))
  } else {
    return getDBInstances()
  }
}

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
          throw new Error('Instance not found or invalid (no host)')
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
}
