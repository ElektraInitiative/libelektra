import { successResponse, errorResponse } from './utils'

import {
  getClusters, createCluster,
  getCluster, updateCluster, deleteCluster,
  getClusterInstances, addInstanceToCluster, removeInstanceFromCluster,
  getInstance
} from '../db'

import remoteKdb from '../connector'

// execute `fn` on all instances in the specified cluster
const applyToAllInstances = (res, clusterId, fn) =>
  getClusterInstances(clusterId)
    .then(instanceIds => Promise.all(instanceIds, getInstance))
    .then(instances => fn(instance))
    .then(output => successResponse(res, output))
    .catch(err => errorResponse(res, err))

export default function initClusterRoutes (app) {
  app.route('/clusters')
    .get((req, res) =>
      getClusters()
        .then(output => successResponse(res, output))
        .catch(err => errorResponse(res, err))
    )
    .post((req, res) =>
      createCluster(req.body)
        .then(output => successResponse(res, output))
        .catch(err => errorResponse(res, err))
    )

  app.route('/clusters/:id')
    .get((req, res) =>
      getCluster(req.params.id)
        .then(output => successResponse(res, output))
        .catch(err => errorResponse(res, err))
    )
    .put((req, res) =>
      updateCluster(req.params.id, req.body)
        .then(output => successResponse(res, output))
        .catch(err => errorResponse(res, err))
    )
    .delete((req, res) =>
      deleteCluster(req.params.id)
        .then(output => successResponse(res, output))
        .catch(err => errorResponse(res, err))
    )

  app.route('/clusters/:id/instances')
    .get((req, res) =>
      applyToAllInstances(res, req.params.id, (instance) => instance)
    )
    .post((req, res) =>
      addInstanceToCluster(req.params.id, req.body && req.body.instanceId)
        .then(output => successResponse(res, output))
        .catch(err => errorResponse(res, err))
    )

  app.delete('/clusters/:id/instances/:instanceId', (req, res) =>
    removeInstanceFromCluster(req.params.id, req.params.instanceId)
      .then(output => successResponse(res, output))
      .catch(err => errorResponse(res, err))
  )

  app.get('/clusters/:id/version', (req, res) =>
    applyToAllInstances(res, req.params.id, (instance) =>
      remoteKdb.version(instance.host)
    )
  )

  // TODO: implement cluster kdb
  app.get('/clusters/:id/kdb', (req, res) =>
    responseCallback(res)(null, {
      ls: [ "user/test" ],
    })
  )
}
