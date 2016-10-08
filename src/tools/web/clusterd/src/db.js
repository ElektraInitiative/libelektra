import kdb from '../../kdb'
import { v4 as generateId } from 'node-uuid'

const ROOT_PATH = 'user/sw/elektra/web/#0/current'

const path = (path) =>
  ROOT_PATH + '/' + path

const findById = (id) =>
  elements => elements.find(
    element => element.id === id
  )

const updateById = (id, update) =>
  elements => elements.reduce(
    (acc, element) => {
      if (element.id === id) {
        acc.push(update(element))
      } else {
        acc.push(element)
      }
      return acc
    }, []
  )

const deleteById = (id) =>
  elements => elements.filter(
    element => element.id !== id
  )

// instances
export const getInstances = () =>
  kdb.export(path('instances'))
    .then(res => Array.isArray(res) ? res : [])

const persistInstances = (instances) =>
  kdb.import(path('instances'), instances)

export const createInstance = ({ name, host }) => {
  const generatedId = generateId()
  return getInstances()
    .then(instances => instances.concat({ id: generatedId, name, host }))
    .then(persistInstances)
    .then(findById(generatedId))
}

export const getInstance = (id) =>
  getInstances()
    .then(findById(id))

export const updateInstance = (id, data) =>
  getInstances()
    .then(updateById(id, (instance) => {
      return { ...instance, ...data }
    }))
    .then(persistInstances)
    .then(findById(id))

export const deleteInstance = (id) =>
  getInstances()
    .then(deleteById(id))
    .then(persistInstances)
    .then(instances => {
      return { id } // return id of deleted instance
    })

// clusters
export const getClusters = () =>
  kdb.export(path('clusters'))
    .then(res => Array.isArray(res) ? res : [])

const persistClusters = (clusters) =>
  kdb.import(path('clusters'), clusters)

export const createCluster = ({ name, instances = [] }) => {
  const generatedId = generateId()
  return getClusters()
    .then(clusters => clusters.concat({ id: generatedId, name, instances }))
    .then(persistClusters)
    .then(findById(generatedId))
}

export const getCluster = (id) =>
  getClusters()
    .then(findById(id))

export const updateCluster = (id, data) =>
  getClusters()
    .then(updateById(id, (cluster) => {
      return { ...cluster, ...data }
    }))
    .then(persistClusters)
    .then(findById(id))

export const deleteCluster = (id) =>
  getClusters()
    .then(deleteById(id))
    .then(persistClusters)
    .then(clusters => {
      return { id } // return id of deleted cluster
    })

export const getClusterInstances = (id) =>
  getCluster(id)
    .then(cluster => cluster.instances)

export const addInstanceToCluster = (id, instanceId) =>
  getClusters()
    .then(updateById(id, (cluster) => {
      return { ...cluster, instances: [ ...cluster.instances, instanceId ] }
    }))
    .then(persistClusters)
    .then(findById(id))

export const removeInstanceFromCluster = (id, instanceId) =>
  getClusters()
    .then(updateById(id, (cluster) => {
      return {
        ...cluster,
        instances: cluster.instances.filter(instance => instance !== instanceId)
      }
    }))
    .then(persistClusters)
    .then(findById(id))
