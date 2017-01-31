/* app.js
this is where the express app is being created and modules and routes are loaded
*/

import express from 'express'
import bodyParser from 'body-parser'

import initRoutes from './routes'
import { PORT } from './config'

export default function initApp (cb) {
  const app = express() // create the express app
  app.use(bodyParser.text()) // parse raw text body from HTTP request

  initRoutes(app) // initialize routes

  app.listen(PORT, () => cb(PORT)) // serve API at PORT
}
