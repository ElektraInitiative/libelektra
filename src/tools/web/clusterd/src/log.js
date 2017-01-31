/* log.js
exports a function that creates logging functions, e.g.
  const { info, error } = makeLog('routes')
  info('routes loaded!') // displays "clusterd:routes:info routes loaded!"
*/

import dude from 'debug-dude'

import { name } from '../package.json'
export const namespace = name.split('/').pop()

export default function makeLog (name) {
  const appendName = name ? (':' + name) : ''
  return dude(namespace + appendName)
}
