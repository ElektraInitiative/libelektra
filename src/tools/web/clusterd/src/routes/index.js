import { responseCallback } from './utils'

import { version as packageVersion } from '../../package.json'
const VERSIONS = { api: packageVersion.split('.').shift() }

import initInstanceRoutes from './instances'
import initClusterRoutes from './clusters'

export default function initRoutes (app) {
  app.get('/version', (req, res) =>
    responseCallback(res)(VERSIONS)
  )

  initInstanceRoutes(app)
  initClusterRoutes(app)
}
