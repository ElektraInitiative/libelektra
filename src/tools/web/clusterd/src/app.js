import express from 'express'
import bodyParser from 'body-parser'
import cors from 'cors'

import initRoutes from './routes'

const PORT = 1235

export default function initApp (cb) {
  const app = express()
  app.use(bodyParser.json())
  app.use(bodyParser.urlencoded({ extended: true }))
  app.use(bodyParser.text()) // for kdb commands
  app.use(cors()) // TODO: don't use cors, serve client from clusterd

  initRoutes(app)

  app.listen(PORT, () => cb(PORT))
}
