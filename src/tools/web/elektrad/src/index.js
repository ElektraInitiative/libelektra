/**
 * @file
 *
 * @brief the main entry point, this file gets executed when elektrad is started
 *
 * it will detect if Elektra is installed and show an error if that isn't the
 * case. otherwise, it will show version information from Elektra and initialize
 * elektrad
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import makeLog from './log'
const { info, error } = makeLog()

import { name as packageName, version as packageVersion } from '../package.json'
import getVersions from './versions'
import initApp from './app'
import kdb from '../../kdb'

info(`%s v%s starting`, packageName, packageVersion)
getVersions()
  .then(versions => {
    if (!versions.elektra) {
      error(`couldn't detect elektra version`)
      error(`are you sure you have libelektra and kdb installed?`)
      process.exit(1)
    } else {
      if (kdb.KDB_COMMAND === 'kdb') info(`|- using default kdb command`)
      else info(`|- using kdb from: ${kdb.KDB_COMMAND}`)
      info(`|- versions: %o`, versions)
      initApp(port => info(`\`-> running on http://localhost:${port}`))
    }
  })
  .catch(err => error(`error while starting %s: %o`, packageName, err))
