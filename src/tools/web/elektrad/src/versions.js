/* versions.js
exports a function to get the API and elektra versions, e.g.
  { api: 1, elektra: { version: '0.8.19', major: 0, minor: 8, micro: 19 } }
*/

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
