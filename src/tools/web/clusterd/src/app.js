/**
 * @file
 *
 * @brief this is where the express app is created and modules/routes loaded
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

import express from 'express'
import bodyParser from 'body-parser'
import serveClient from './serveClient'
import path from 'path'

import initRoutes from './routes'
import { PORT } from './config'

export default function initApp (cb) {
  const app = express() // create the express app

  app.use(bodyParser.json()) // parse json body
  app.use(bodyParser.urlencoded({ extended: true })) // parse urlencoded body
  app.use(bodyParser.text()) // parse raw text body, for kdb commands

  initRoutes(app) // initialize routes

  // serve the client
  app.use(serveClient({ path: path.join(__dirname, '/../../client') }))

  app.listen(PORT, () => cb(PORT)) // serve API at PORT
}
