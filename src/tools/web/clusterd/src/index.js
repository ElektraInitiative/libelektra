/**
 * @file
 *
 * @brief the main entry point, this file gets executed when clusterd is started
 *
 * it will detect if Elektra (with the yajl plugin) is installed and show an
 * error if that isn't the case. otherwise, it will show version information
 * from Elektra and initialize clusterd
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import makeLog from './log'
const { info, error } = makeLog()

import { name as packageName, version as packageVersion } from '../package.json'
import getVersions from './versions'
import initApp from './app'

import { getInstances } from './db'

info(`%s v%s starting`, packageName, packageVersion)
getVersions()
  .then(versions => {
    if (!versions.elektra) {
      error(`couldn't detect elektra version`)
      error(`are you sure you have libelektra and kdb installed?`)
      process.exit(1)
    } else {
      getInstances() // make sure yajl is installed
        .then(() => {
          info(`|- versions: %o`, versions)
          initApp(port => info(`\`-> running on http://localhost:${port}`))
        })
        .catch((err) => {
          if (err.message.indexOf('Was not able to load such a plugin!')) {
            error(`missing dependencies`)
            error(`the yajl plugin is not installed for libelektra`)
            process.exit(1)
          } else throw err // re-throw error
        })
    }
  })
  .catch(err => error(`error while starting %s: %o`, packageName, err))
