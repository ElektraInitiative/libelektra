import parallel from 'async/parallel'
import { responseCallback } from './utils'

import kdb from '../kdb'
import VERSIONS from '../versions'

// TODO: handle errors

const kdbGet = (path, cb) =>
  parallel({
    ls: (localCallback) => kdb.ls(path, localCallback),
    value: (localCallback) => kdb.get(path, localCallback),
    // TODO: meta: (localCallback) => kdb.getmeta(path, localCallback),
  }, (err, res) => {
    if (err) return cb(err)
    cb(null, {
      ls: res.ls,
      value: res.value && res.value[0] && res.value[0].length > 0
        ? res.value[0]
        : undefined
    })
  })

export default function initRoutes (app) {
  app.get('/version', (req, res) =>
    responseCallback(res)(null, VERSIONS)
  )

  app.get('/kdb', (req, res) =>
    kdbGet('/', responseCallback(res))
  )

  app.route('/kdb/*')
    .get((req, res) =>
      kdbGet(req.params[0], responseCallback(res))
    )
    .put((req, res) =>
      kdb.set(req.params[0], req.body, responseCallback(res))
    )
    .delete((req, res) =>
      kdb.rm(req.params[0], responseCallback(res))
    )
}
