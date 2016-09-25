import makeLog from '../log'
const { error } = makeLog('routes:instances')

import { responseCallback } from './utils'

import {
  getInstances, createInstance,
  getInstance, updateInstance, deleteInstance,
} from '../db'

import kdb from '../connector'

// TODO: validate user input!
// TODO: handle errors

export default function initInstanceRoutes (app) {
  app.route('/instances')
    .get((req, res) =>
      getInstances(responseCallback(res))
    )
    .post((req, res) =>
      createInstance(req.body, responseCallback(res))
    )

  app.route('/instances/:id')
    .get((req, res) =>
      getInstance(req.params.id, responseCallback(res))
    )
    .put((req, res) =>
      updateInstance(req.params.id, req.body, responseCallback(res))
    )
    .delete((req, res) =>
      deleteInstance(req.params.id, responseCallback(res))
    )

  app.get('/instances/:id/version', (req, res) =>
    getInstance(req.params.id, (instance) =>
      kdb.version(instance.host)
        .then(responseCallback(res))
        .catch(err => error('connection error: %o', err))
    )
  )

  app.get('/instances/:id/kdb', (req, res) =>
    getInstance(req.params.id, (instance) =>
      kdb.ls(instance.host)
        .then(responseCallback(res))
        .catch(err => error('connection error: %o', err))
    )
  )

  app.route('/instances/:id/kdb/*')
    .get((req, res) =>
      getInstance(req.params.id, (instance) =>
        kdb.get(instance.host, req.params[0])
          .then(responseCallback(res))
          .catch(err => error('connection error: %o', err))
      )
    )
    .put((req, res) =>
      getInstance(req.params.id, (instance) =>
        kdb.set(instance.host, req.params[0], req.body)
          .then(responseCallback(res))
          .catch(err => error('connection error: %o', err))
      )
    )
    .delete((req, res) =>
      getInstance(req.params.id, (instance) =>
        kdb.rm(instance.host, req.params[0])
          .then(responseCallback(res))
          .catch(err => error('connection error: %o', err))
      )
    )
}
