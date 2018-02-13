/**
 * @file
 *
 * @brief small library to access Elektra’s kdb via node.js
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

const { exec } = require('child_process')
const { readFileSync, unlink } = require('fs')

// constants
const ERR_KEY_NOT_FOUND = 'Did not find key'

const ERROR_REGEX = /Sorry, the error \(\#([0-9]+)\) occurred/
const DESCRIPTION_REGEX = /Description: (.*)$/
const INGROUP_REGEX = /Ingroup: (.*)$/
const MODULE_REGEX = /Module: (.*)$/
const AT_REGEX = /At: (.*)$/
const REASON_REGEX = /Reason: (.*)$/
const MOUNTPOINT_REGEX = /Mountpoint: (.*)$/
const CONFIGFILE_REGEX = /Configfile: (.*)$/

function parseError (message) {
  let error = {}
  for (let line of message.split('\n')) {
    let res
    if (res = line.match(ERROR_REGEX)) {
      error.num = Number(res[1])
    } else if (currentError !== false) {
      if (res = line.match(DESCRIPTION_REGEX)) {
        currentError.description = res[1]
      } else if (res = line.match(INGROUP_REGEX)) {
        currentError.ingroup = res[1]
      } else if (res = line.match(MODULE_REGEX)) {
        currentError.module = res[1]
      } else if (res = line.match(AT_REGEX)) {
        currentError.at = res[1]
      } else if (res = line.match(REASON_REGEX)) {
        currentError.reason = res[1]
      } else if (res = line.match(MOUNTPOINT_REGEX)) {
        currentError.mountpoint = res[1]
      } else if (res = line.match(CONFIGFILE_REGEX)) {
        currentError.configfile = res[1]
      }
    }
  }
  return error
}

// KDBError
function KDBError (message) {
    this.name = 'KDBError'
    let isError = false
    for (let line of message.split('\n')) {
      let res
      if (res = line.match(ERROR_REGEX)) {
        this.num = Number(res[1])
        isError = true
      } else if (isError) {
        if (res = line.match(DESCRIPTION_REGEX)) {
          this.description = res[1]
        } else if (res = line.match(INGROUP_REGEX)) {
          this.ingroup = res[1]
        } else if (res = line.match(MODULE_REGEX)) {
          this.module = res[1]
        } else if (res = line.match(AT_REGEX)) {
          this.at = res[1]
        } else if (res = line.match(REASON_REGEX)) {
          this.reason = res[1]
        } else if (res = line.match(MOUNTPOINT_REGEX)) {
          this.mountpoint = res[1]
        } else if (res = line.match(CONFIGFILE_REGEX)) {
          this.configfile = res[1]
        }
      }
    }
    if (this.description) {
      this.message = this.description + (
        this.reason ? ': ' + this.reason : ''
      )
    } else {
      this.message = message || ''
    }
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
      if (errors.length > 1 && errors[1] === ERR_KEY_NOT_FOUND) {
        return resolve()
      } else {
        return reject(new KDBError(err.message))
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
    let val = values[i - 1].replace(/((\\\\)*)(\\(")|("))/g, '$1\\$4$5')
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

// move value from given `path` to `destination`
const mv = (path, destination) =>
  safeExec(escapeValues`kdb mv -r ${path} ${destination}`)

// copy value from given `path` to `destination`
const cp = (path, destination) =>
  safeExec(escapeValues`kdb cp -r ${path} ${destination}`)

// remove value at given `path`
const rm = (path) =>
  safeExec(escapeValues`kdb rm -r ${path}`)

// list meta values at given `path`
const lsmeta = (path) =>
  safeExec(escapeValues`kdb lsmeta ${path}`)
    .then(stdout => stdout && stdout.split('\n'))

// get meta value from given `path`
const getmeta = (path, meta) =>
  safeExec(escapeValues`kdb getmeta ${path} ${meta}`)

// set meta value at given `path`
const setmeta = (path, meta, value) =>
  safeExec(escapeValues`kdb setmeta ${path} ${meta} ${value}`)

// remove meta value at given `path`
const rmmeta = (path, meta) =>
  safeExec(escapeValues`kdb rmmeta ${path} ${meta}`)

// get all metavalues for given `path`
const getAllMeta = (path) =>
  lsmeta(path)
    .then(metaValues => metaValues && Promise.all(
      metaValues.map(meta =>
        getmeta(path, meta).then(val => {
          return { [meta]: val }
        })
      )
    ))
    // merge objects
    .then(resolvedMetaValues =>
      resolvedMetaValues && Object.assign.apply(Object, resolvedMetaValues)
    )

const getBufferFile = () => `/tmp/elektra-web.${Date.now()}.buffer.json`

// export javascript object from given `path`
const _export = (path) => {
  const buffer = getBufferFile()
  return safeExec(escapeValues`kdb export ${path} yajl ${buffer}`)
    .then(() => {
      const data = JSON.parse(readFileSync(buffer))
      unlink(buffer, (err) =>
        err && console.error('could not delete buffer file:', err)
      )
      return data
    })
}

// import javascript object at given `path`
const _import = (path, value) =>
  safeExec(
    // we can trust JSON.stringify to escape values for us
    `echo '${JSON.stringify(value)}' | ` + // pipe json into kdb
    escapeValues`kdb import ${path} yajl`
  ).then(result => _export(path))

// get value and available paths under a given `path`
const getAndLs = (path) =>
  Promise.all(
    [ ls(path), get(path), getAllMeta(path) ] // execute ls and get in parallel
  ).then(([ lsRes, value, meta ]) => {
    return { ls: lsRes || [], value, meta } // return results as object
  })

// export kdb functions as `kdb` object
module.exports = {
  version, ls, get, getAndLs, set, mv, cp, rm, export: _export, import: _import,
  getmeta, setmeta, rmmeta, lsmeta, getAllMeta,
}
