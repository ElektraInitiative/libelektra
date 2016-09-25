import dude from 'debug-dude'

import { name as namespace } from '../package.json'

export default function makeLog (name) {
  const appendName = name ? (':' + name) : ''
  return dude(namespace + appendName)
}
