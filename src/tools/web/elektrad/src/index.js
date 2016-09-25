import makeLog from './log'
const { info, error } = makeLog()

import VERSIONS from './versions'
if (!VERSIONS.elektra) {
  error(`couldn't detect elektra version`)
  error(`are you sure you have libelektra and kdb installed?`)
  process.exit(1)
}

import { name as packageName, version as packageVersion } from '../package.json'
info(`%s v%s starting: %o`, packageName, packageVersion, VERSIONS)

import express from 'express'
const app = express()

import bodyParser from 'body-parser'
app.use(bodyParser.text())

import initRoutes from './routes'
initRoutes(app)

app.listen(1234, () =>
  info(`-> running on http://localhost:1234`)
)
