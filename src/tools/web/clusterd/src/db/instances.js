/* db/instances.js
exports database operations regarding instances
*/

import kdb from '../../../kdb'
import { path, generateId, findById, updateById, deleteById } from './utils'

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
