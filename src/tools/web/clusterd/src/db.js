import low from 'lowdb'
import underscoreDb from 'underscore-db'

const db = low('db.json')
db._.mixin(underscoreDb)

const DB_SCHEME = {
  instances: [],
  clusters: [],
}
db.defaults(DB_SCHEME).value()

// TODO: use lowdb async storage here

export const getInstances = (cb) => cb(
  db.get('instances')
    .value()
)

export const createInstance = ({ name, host }, cb) => cb(
  db.get('instances')
    .insert({ name, host })
    .value()
)

export const getInstance = (id, cb) => cb(
  db.get('instances')
    .getById(id)
    .value()
)

export const updateInstance = (id, data, cb) => cb(
  db.get('instances')
    .getById(id)
    .assign(data)
    .value()
)

export const deleteInstance = (id, cb) => cb(
  db.get('instances')
    .remove({ id })
    .first()
    .value()
)
