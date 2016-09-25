import makeLog from './log'
const { error } = makeLog('versions')

import { version } from '../package.json'
const API_VERSION = version.split('.').shift()

import { execSync } from 'child_process'

const ELEKTRA_VERSION_REGEX = /KDB_VERSION\:\ ([0-9]+\.[0-9]+\.[0-9]+)\n/
const getElektraVersion = () => {
  let output
  try {
    output = execSync(`kdb --version`, { stdio: 'pipe' })
  } catch (err) {
    const msg = err.message.split('\n')
    msg.pop() // remove empty line
    error(msg.join(' ~ '))
    return
  }

  const matches = ELEKTRA_VERSION_REGEX.exec(output)
  if (matches.length > 1 && matches[1]) {
    return matches[1]
  }
}

const ELEKTRA_VERSION = getElektraVersion()

export default { api: Number(API_VERSION), elektra: ELEKTRA_VERSION }
