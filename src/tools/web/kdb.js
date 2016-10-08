// kdb.js - small library to access Elektra's kdb via node.js
import { exec } from 'child_process'

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
const safeExec = (script, cb) =>
  exec(script, (err, stdout, stderr) => {
    if (err) {
      const errors = err.message.split('\n')
      // ignore error if it's "key not found"
      if (!(errors.length > 1 && errors[1] === ERR_KEY_NOT_FOUND)) {
        return cb(err)
      }
    }
    if (stderr && stderr !== ERR_KEY_NOT_FOUND) {
      return cb(new KDBError(stderr))
    }
    return cb(null, trimNewline(stdout), stderr)
  })

// escape strings by surrounding them with ""
const escapeValues = (template, ...values) =>
  template.reduce((acc, part, i) => {
    let val = values[i - 1]
    if (typeof val === 'string') val = `"${val}"`
    return acc + val + part
  })

// list available paths under a given `path`
const ls = (path, cb) =>
  safeExec(`kdb ls ${path}`, (err, stdout) => {
    if (err) return cb(err)
    let paths = stdout.split('\n')
    return cb(null, paths)
  })

// get value from given `path`
const get = (path, cb) =>
  safeExec(escapeValues`kdb get ${path}`, cb)

// set value at given `path`
const set = (path, value, cb) =>
  safeExec(escapeValues`kdb set ${path} ${value}`, cb)

// remove value at given `path`
const rm = (path, cb) =>
  safeExec(escapeValues`kdb rm ${path}`, cb)

// export kdb functions as `kdb` object
export default { ls, get, set, rm }
