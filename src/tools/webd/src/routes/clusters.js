/**
 * @file
 *
 * @brief this exports a function that defines routes for the express app
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import makeLog from "../log";
const { error } = makeLog("routes:clusters");

import { successResponse, errorResponse } from "./utils";

import {
  getClusters,
  createCluster,
  getCluster,
  updateCluster,
  deleteCluster,
  getClusterInstances,
  addInstanceToCluster,
  removeInstanceFromCluster,
  getInstance,
  virtualKdb,
} from "../db";

import remoteKdb from "../connector";
import kdb from "../kdb";

// execute `fn` on all instances in the specified cluster
const applyToAllInstances = (res, clusterId, fn) =>
  getClusterInstances(clusterId)
    .then((instanceIds) => Promise.all(instanceIds.map(getInstance)))
    .then((instances) => Promise.all(instances.map(fn)))
    .then((output) => successResponse(res, output))
    .catch((err) => {
      error("error while applying to all instances:", err);
      return errorResponse(res, err);
    });

export default function initClusterRoutes(app) {
  app
    .route("/api/clusters")
    .get((req, res) =>
      getClusters()
        .then((output) => successResponse(res, output))
        .catch((err) => errorResponse(res, err))
    )
    .post((req, res) =>
      createCluster(req.body)
        .then((output) => successResponse(res, output))
        .catch((err) => errorResponse(res, err))
    );

  app
    .route("/api/clusters/:id")
    .get((req, res) =>
      getCluster(req.params.id)
        .then((output) => successResponse(res, output))
        .catch((err) => errorResponse(res, err))
    )
    .put((req, res) =>
      updateCluster(req.params.id, req.body)
        .then((output) => successResponse(res, output))
        .catch((err) => errorResponse(res, err))
    )
    .delete((req, res) =>
      deleteCluster(req.params.id)
        .then((output) => successResponse(res, output))
        .catch((err) => errorResponse(res, err))
    );

  app
    .route("/api/clusters/:id/instances")
    .get((req, res) =>
      applyToAllInstances(res, req.params.id, (instance) => instance)
    )
    .post((req, res) =>
      addInstanceToCluster(req.params.id, req.body && req.body.instanceId)
        .then((output) => successResponse(res, output))
        .catch((err) => errorResponse(res, err))
    );

  app.delete("/api/clusters/:id/instances/:instanceId", (req, res) =>
    removeInstanceFromCluster(req.params.id, req.params.instanceId)
      .then((output) => successResponse(res, output))
      .catch((err) => errorResponse(res, err))
  );

  app.get("/api/clusters/:id/version", (req, res) =>
    applyToAllInstances(res, req.params.id, (instance) =>
      remoteKdb.version(instance.host)
    )
  );

  // cluster kdb

  const stripVirtualPath = (clusterId) => (output) => {
    const strippedLs =
      Array.isArray(output.ls) &&
      output.ls.map(
        (path) => path.replace(virtualKdb(clusterId) + "/", "") // strip virtual kdb root from path
      );
    return { ...output, ls: strippedLs };
  };

  // TODO: check if cluster id actually exists
  app.get("/api/clusters/:id/kdb", (req, res) =>
    kdb
      .getAndLs(virtualKdb(req.params.id))
      .then(stripVirtualPath(req.params.id))
      .then((output) => successResponse(res, output))
      .catch((err) => errorResponse(res, err))
  );

  // TODO: what if an instance isn't online? -> send later
  app
    .route("/api/clusters/:id/kdb/*")
    .get((req, res) =>
      kdb
        .getAndLs(virtualKdb(req.params.id, req.params[0]))
        .then(stripVirtualPath(req.params.id))
        .then((output) => successResponse(res, output))
        .catch((err) => errorResponse(res, err))
    )
    .put((req, res) => {
      const clusterId = req.params.id;
      const path = req.params[0];
      // set key in virtual kdb on webd server
      kdb
        .set(virtualKdb(clusterId, path), req.body)
        .then((output) =>
          // set key on all instances in the cluster
          applyToAllInstances(res, clusterId, (instance) =>
            remoteKdb.set(instance.host, path, req.body)
          )
        )
        .catch((err) => errorResponse(res, err));
    })
    .delete((req, res) => {
      const clusterId = req.params.id;
      const path = req.params[0];
      kdb
        .rm(virtualKdb(clusterId, path))
        .then((output) =>
          // set key on all instances in the cluster
          applyToAllInstances(res, clusterId, (instance) =>
            remoteKdb.rm(instance.host, path)
          )
        )
        .catch((err) => errorResponse(res, err));
    });
}
