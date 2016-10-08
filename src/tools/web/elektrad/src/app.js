import express from 'express'
import bodyParser from 'body-parser'

import initRoutes from './routes'

const PORT = 1234

export default function initApp (cb) {
  const app = express()
  app.use(bodyParser.text())

  initRoutes(app)

  app.listen(PORT, () => cb(PORT))
}
