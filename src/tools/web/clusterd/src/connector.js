/**
 * @file
 *
 * @brief exports function stubs to access elektrad remotely
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

import fetch from 'node-fetch'

const encodePath = (path) =>
  path.split('/').map(encodeURIComponent).join('/')

const version = (host) =>
  fetch(`${host}/version`)
    .then(res => res.json())

const getRoot = (host) =>
  fetch(`${host}/kdb`)
    .then(res => res.json())

const getPath = (host, path) =>
  fetch(`${host}/kdb/${encodePath(path)}`)
    .then(res => res.json())

const get = (host, path) =>
  path ? getPath(host, path) : getRoot(host)

const set = (host, path, value) =>
  fetch(`${host}/kdb/${encodePath(path)}`,
    {
      method: 'PUT',
      headers: {
        'Content-Type': 'text/plain',
      },
      body: value,
    }
  )
    .then(res => res.json())

const rm = (host, path) =>
  fetch(`${host}/kdb/${encodePath(path)}`, { method: 'DELETE' })
    .then(res => {
      return { status: res.status }
    })

export default { version, get, set, rm }
