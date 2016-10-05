import makeLog from '../log'
const { error } = makeLog('routes:clusters')

import async from 'async'
import { responseCallback } from './utils'

import {
  getClusters, createCluster,
  getCluster, updateCluster, deleteCluster,
  getClusterInstances, addInstanceToCluster, removeInstanceFromCluster,
  getInstance
} from '../db'

import kdb from '../connector'

// TODO: validate user input!
// TODO: handle errors

const applyToAllInstances = (res, clusterId, fn) =>
  getClusterInstances(clusterId, (err, instanceIds) => // get a list of instanceIds
    async.map(instanceIds, getInstance, (err, instances) => // resolve instanceIds to instance objects
      async.map(instances,
        (instance, cb) => // apply `fn` to all instances
          fn(instance)
            .then(res => cb(null, res))
            .catch(err => cb(err)),
        responseCallback(res) // return the error/results via express
      )
    )
  )

export default function initClusterRoutes (app) {
  app.route('/clusters')
    .get((req, res) =>
      getClusters(responseCallback(res))
    )
    .post((req, res) =>
      createCluster(req.body, responseCallback(res))
    )

  app.route('/clusters/:id')
    .get((req, res) =>
      getCluster(req.params.id, responseCallback(res))
    )
    .put((req, res) =>
      updateCluster(req.params.id, req.body, responseCallback(res))
    )
    .delete((req, res) =>
      deleteCluster(req.params.id, responseCallback(res))
    )

  app.route('/clusters/:id/instances')
    .get((req, res) =>
      applyToAllInstances(res, req.params.id, (instance) => Promise.resolve(instance))
    )
    .post((req, res) =>
      addInstanceToCluster(req.params.id, req.body && req.body.instanceId, responseCallback(res))
    )

  app.delete('/clusters/:id/instances/:instanceId', (req, res) =>
    removeInstanceFromCluster(req.params.id, req.params.instanceId, responseCallback(res))
  )

  app.get('/clusters/:id/version', (req, res) =>
    applyToAllInstances(res, req.params.id, (instance) =>
      kdb.version(instance.host)
    )
  )

  app.get('/clusters/:id/kdb', (req, res) =>
    responseCallback(res)(null, {
      ls: [ "user/test" ],
    }) // TODO
  )

  // TODO
  // app.get('/clusters/:id/kdb', (req, res) =>
  //   getCluster(req.params.id, (instance) =>
  //     kdb.ls(instance.host)
  //       .then(responseCallback(res))
  //       .catch(err => error('connection error: %o', err))
  //   )
  // )
  //
  // app.route('/clusters/:id/kdb/*')
  //   .get((req, res) =>
  //     getCluster(req.params.id, (instance) =>
  //       kdb.get(instance.host, req.params[0])
  //         .then(responseCallback(res))
  //         .catch(err => error('connection error: %o', err))
  //     )
  //   )
  //   .put((req, res) =>
  //     getCluster(req.params.id, (instance) =>
  //       kdb.set(instance.host, req.params[0], req.body)
  //         .then(responseCallback(res))
  //         .catch(err => error('connection error: %o', err))
  //     )
  //   )
  //   .delete((req, res) =>
  //     getCluster(req.params.id, (instance) =>
  //       kdb.rm(instance.host, req.params[0])
  //         .then(responseCallback(res))
  //         .catch(err => error('connection error: %o', err))
  //     )
  //   )
}
