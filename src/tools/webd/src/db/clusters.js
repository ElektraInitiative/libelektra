/**
 * @file
 *
 * @brief exports database operations regarding clusters
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import kdb from "../kdb";
import { path, generateId, findById, updateById, deleteById } from "./utils";

export const getClusters = () =>
  kdb.export(path("clusters")).then((res) => (Array.isArray(res) ? res : []));

const persistClusters = (clusters) => kdb.import(path("clusters"), clusters);

export const createCluster = ({ name, instances = [] }) => {
  const generatedId = generateId();
  return getClusters()
    .then((clusters) => clusters.concat({ id: generatedId, name, instances }))
    .then(persistClusters)
    .then(findById(generatedId));
};

export const getCluster = (id) => getClusters().then(findById(id));

export const updateCluster = (id, data) =>
  getClusters()
    .then(
      updateById(id, (cluster) => {
        return { ...cluster, ...data };
      })
    )
    .then(persistClusters)
    .then(findById(id));

export const deleteCluster = (id) =>
  getClusters()
    .then(deleteById(id))
    .then(persistClusters)
    .then((clusters) => {
      return { id }; // return id of deleted cluster
    });

export const getClusterInstances = (id) =>
  getCluster(id).then((cluster) => cluster.instances);

export const addInstanceToCluster = (id, instanceId) =>
  getClusters()
    .then(
      updateById(id, (cluster) => {
        return { ...cluster, instances: [...cluster.instances, instanceId] };
      })
    )
    .then(persistClusters)
    .then(findById(id));

export const removeInstanceFromCluster = (id, instanceId) =>
  getClusters()
    .then(
      updateById(id, (cluster) => {
        return {
          ...cluster,
          instances: cluster.instances.filter(
            (instance) => instance !== instanceId
          ),
        };
      })
    )
    .then(persistClusters)
    .then(findById(id));
