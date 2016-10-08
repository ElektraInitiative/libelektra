import { version } from '../package.json'
// api version is the major version of the package.json version
const API_VERSION = version.split('.').shift()

import kdb from '../../kdb'

export default function getVersions () {
  return kdb.version()
    .then(elektraVersions => {
      return { api: Number(API_VERSION), elektra: elektraVersions }
    })
}
