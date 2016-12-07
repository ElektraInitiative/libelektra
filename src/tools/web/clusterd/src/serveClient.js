import makeLog from './log'
const { info } = makeLog('client')

import express from 'express'
import { join as joinPath } from 'path'
import { existsSync, readFileSync } from 'fs'

// adapted from https://github.com/rangle/serve-webpack-client
export default function createRouter ({
  path, index = 'index.html', config = 'webpack.config.js', dist = 'dist',
}) {
  const router = new express.Router()

  if (!path) {
    throw new Error('please specify a path')
  }

  const distPath = joinPath(path, dist)
  const webpackConfig = require(joinPath(path, config))

  if (process.env.NODE_ENV === 'production') {
    // In production, assets are bundled at build time and served statically from
    // the 'dist' folder. This is more efficient.
    info('prod mode: serving client static assets')
    router.use(express.static(distPath))
    router.get('*', (req, res) => res.sendFile(joinPath(distPath, index)))
  } else {
    // In development, assets are bundled and hot-loaded on the fly. This is
    // resource intensive, but allows auto-rebuilding of client and server code
    // for developer convenience.
    info('dev mode: serving client from webpack... please wait')

    const requireClient = (module) =>
      require(joinPath(path, 'node_modules', module))

    const webpack = requireClient('webpack')

    const compiler = webpack(webpackConfig)
    const devMiddleware = requireClient('webpack-dev-middleware')(compiler, {
      noInfo: true,
    })

    const loadAndCacheFile = (req, res) => {
      const file = (req.path === '/') ? '/index.html' : req.path
      const filePath = joinPath(path, file)

      // load index file into memory-fs if it does not exist yet
      if (!devMiddleware.fileSystem.existsSync(filePath)) {
        // if file really doesn't exist, show 404
        if (!existsSync(filePath)) return res.status(404).end()
        // otherwise, create the folder
        const folderPath = filePath.split('/').slice(0, -1).join('/')
        devMiddleware.fileSystem.mkdirpSync(folderPath)
        // then create the file
        devMiddleware.fileSystem.writeFileSync(filePath, readFileSync(filePath))
      }

      return res.end(devMiddleware.fileSystem.readFileSync(filePath))
    }

    router.use(devMiddleware)
    router.use(requireClient('webpack-hot-middleware')(compiler))
    router.get('*', loadAndCacheFile)
  }

  return router
}
