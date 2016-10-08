// kdb.js - small library to access Elektra's kdb via node.js
const { exec } = require('child_process')

// constants
const ERR_KEY_NOT_FOUND = 'Did not find key'

// KDBError
function KDBError (message) {
    this.name = 'KDBError'
    this.message = message || ''
}
KDBError.prototype = Error.prototype

// remove newline from the end of a string
const trimNewline = (str) =>
  str.substring(0, str.length - 1)

// execute a script while catching and parsing errors
const safeExec = (script) => new Promise((resolve, reject) =>
  exec(script, (err, stdout, stderr) => {
    if (err) {
      const errors = err.message.split('\n')
      // ignore error if it's "key not found"
      if (!(errors.length > 1 && errors[1] === ERR_KEY_NOT_FOUND)) {
        return reject(err)
      }
    }
    if (stderr) {
      if (stderr !== ERR_KEY_NOT_FOUND) {
        return reject(new KDBError(stderr))
      } else {
        return resolve() // no key found, return no value
      }
    }
    const result = trimNewline(stdout)
    if (!result) {
      return resolve() // empty result, return no value
    }
    return resolve(result)
  })
)

// escape strings by surrounding them with ""
const escapeValues = (template, ...values) =>
  template.reduce((acc, part, i) => {
    let val = values[i - 1]
    if (typeof val === 'string') val = `"${val}"`
    return acc + val + part
  })

// list available paths under a given `path`
const ls = (path) =>
  safeExec(escapeValues`kdb ls ${path}`)
    .then(stdout => stdout && stdout.split('\n'))

// get value from given `path`
const get = (path) =>
  safeExec(escapeValues`kdb get ${path}`)

// set value at given `path`
const set = (path, value) =>
  safeExec(escapeValues`kdb set ${path} ${value}`)

// remove value at given `path`
const rm = (path) =>
  safeExec(escapeValues`kdb rm ${path}`)

// export javascript object from given `path`
const _export = (path) =>
  safeExec(escapeValues`kdb export ${path} yajl`)
    .then(stdout => stdout && JSON.parse(stdout))

// import javascript object at given `path`
const _import = (path, value) =>
  safeExec(
    `echo "${encodeURIComponent(JSON.stringify(value))}" | ` + // pipe json into kdb
    escapeValues`kdb import ${path} yajl`
  )

// export kdb functions as `kdb` object
module.exports = { ls, get, set, rm, export: _export, import: _import }
