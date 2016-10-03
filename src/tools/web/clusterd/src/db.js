import low from 'lowdb'
import underscoreDb from 'underscore-db'

const db = low('db.json')
db._.mixin(underscoreDb)

const DB_SCHEME = {
  instances: [],
  clusters: [],
}
db.defaults(DB_SCHEME).value()

// instances
export const getInstances = (cb) => cb(null,
  db.get('instances')
    .value()
)

export const createInstance = ({ name, host }, cb) => cb(null,
  db.get('instances')
    .insert({ name, host })
    .value()
)

export const getInstance = (id, cb) => cb(null,
  db.get('instances')
    .getById(id)
    .value()
)

export const updateInstance = (id, data, cb) => cb(null,
  db.get('instances')
    .getById(id)
    .assign(data)
    .value()
)

export const deleteInstance = (id, cb) => cb(null,
  db.get('instances')
    .remove({ id })
    .first()
    .value()
)

// clusters
export const getClusters = (cb) => cb(null,
  db.get('clusters')
    .value()
)

export const createCluster = ({ name }, cb) => cb(null,
  db.get('clusters')
    .insert({ name, instances: [] })
    .value()
)

export const getCluster = (id, cb) => cb(null,
  db.get('clusters')
    .getById(id)
    .value()
)

export const updateCluster = (id, data, cb) => cb(null,
  db.get('clusters')
    .getById(id)
    .assign(data)
    .value()
)

export const deleteCluster = (id, cb) => cb(null,
  db.get('clusters')
    .remove({ id })
    .first()
    .value()
)

export const getClusterInstances = (id, cb) => cb(null,
  db.get('clusters')
    .getById(id)
    .get('instances')
    .value()
)

export const addInstanceToCluster = (id, instanceId, cb) => cb(null,
  db.get('clusters')
    .getById(id)
    .get('instances')
    .pull(instanceId)
    .push(instanceId)
    .value()
)

export const removeInstanceFromCluster = (id, instanceId, cb) => cb(null,
  db.get('clusters')
    .getById(id)
    .get('instances')
    .pull(instanceId)
    .value()
)
