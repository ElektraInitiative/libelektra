/**
 * @file
 *
 * @brief the main entry point for the database
 *
 * this exports database functions from other files
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

export { getSingleInstanceOption } from "./config";

export {
  getInstances,
  createInstance,
  getInstance,
  updateInstance,
  deleteInstance,
} from "./instances";

export {
  getClusters,
  createCluster,
  getCluster,
  updateCluster,
  deleteCluster,
  getClusterInstances,
  addInstanceToCluster,
  removeInstanceFromCluster,
} from "./clusters";

export { virtualKdb } from "./utils";
