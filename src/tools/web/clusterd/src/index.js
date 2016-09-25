import makeLog from './log'
const { info, error } = makeLog()

import { name as packageName, version as packageVersion } from '../package.json'
info(`%s v%s starting`, packageName, packageVersion)

import express from 'express'
const app = express()

import bodyParser from 'body-parser'
app.use(bodyParser.json())
app.use(bodyParser.urlencoded({ extended: true }))
app.use(bodyParser.text()) // for kdb commands

import cors from 'cors'
app.use(cors()) // TODO: restrict cors access?

import initRoutes from './routes'
initRoutes(app)

app.listen(1235, () =>
  info(`-> running on http://localhost:1235`)
)
