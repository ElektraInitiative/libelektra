import fetch from 'node-fetch'

const encodePath = (path) =>
  path.split('/').map(encodeURIComponent).join('/')

const version = (host) =>
  fetch(`${host}/version`)
    .then(res => res.json())

// TODO: make this accept a path
const ls = (host) =>
  fetch(`${host}/kdb`)
    .then(res => res.json())

const get = (host, path) =>
  fetch(`${host}/kdb/${encodePath(path)}`)
    .then(res => res.json())

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
    .then(res => res.json())

export default { version, ls, get, set, rm }
