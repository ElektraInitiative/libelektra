import express from 'express'
import bodyParser from 'body-parser'
import serveClient from './serveClient'
import path from 'path'

import initRoutes from './routes'
import { PORT } from './config'

export default function initApp (cb) {
  const app = express()

  app.use(bodyParser.json())
  app.use(bodyParser.urlencoded({ extended: true }))
  app.use(bodyParser.text()) // for kdb commands

  initRoutes(app)

  app.use(serveClient({ path: path.join(__dirname, '/../../client') }))

  app.listen(PORT, () => cb(PORT))
}
