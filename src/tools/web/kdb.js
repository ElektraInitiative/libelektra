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
    /*
    Explanation of regular expression:
    - $1 `(\\\\)*` Matches an even number of _backslashes_
    - $4 Matches one _quote_ when there was an odd number of backslashes
    - $5 Matches one _quote_ when there was an even number of backslashes

    For instance in `\\\\\"`, $1 is `\\\\`, $4 is `"` and $5 is an empty string.
    So `\\\\\"` gets replaced to itself.
    In case of an even number of backslashes one backslash is added.

    (source: @krit0n - https://github.com/ElektraInitiative/libelektra/pull/983#discussion_r83965059)
    */
    let val = values[i - 1].replace(/((\\\\)*)(\\(")|("))/g, '$1\\$4$5'))
    if (typeof val === 'string') val = `"${val}"`
    return acc + val + part
  })

const ELEKTRA_VERSION_REGEX = /KDB_VERSION\:\ ([0-9]+\.[0-9]+\.[0-9]+)\n/

// get Elektra version
const version = () =>
  safeExec(`kdb --version`)
    .then(output => { // parse result
      const matches = ELEKTRA_VERSION_REGEX.exec(output)
      if (!matches) {
        throw new Error('invalid version: ' + output)
      }
      return matches
    })
    .then(matches => matches.length > 1 && matches[1]) // select version from matches
    .then(fullVersion => {
      const splitVersions = fullVersion.split('.')
      return {
        version: fullVersion,
        major: Number(splitVersions[0]),
        minor: Number(splitVersions[1]),
        micro: Number(splitVersions[2])
      }
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
    `echo "${JSON.stringify(value).replace(/\"/g, '\\"')}" | ` + // pipe json into kdb
    escapeValues`kdb import ${path} yajl`
  ).then(result => _export(path))

// get value and available paths under a given `path`
const getAndLs = (path) =>
  Promise.all(
    [ ls(path), get(path) ] // execute ls and get in parallel
  ).then(([ lsRes, value ]) => {
    if (lsRes) {
      return { ls: lsRes, value } // return results as object
    }
  })

// export kdb functions as `kdb` object
module.exports = {
  version, ls, get, getAndLs, set, rm, export: _export, import: _import
}
