import makeLog from './log'
const { error } = makeLog()

import { exec } from 'child_process'

const KEY_NOT_FOUND = 'Did not find key'

const trimNewline = (str) =>
  str.substring(0, str.length - 1)

const safeExec = (script, cb) =>
  exec(script, (err, stdout, stderr) => {
    if (err) {
      const errors = err.message.split('\n')
      // ignore error if it's "key not found"
      if (!(errors.length > 1 && errors[1] === KEY_NOT_FOUND)) {
        error('exec error: %o', err)
        return cb(err)
      }
    }
    if (stderr && stderr !== KEY_NOT_FOUND) {
      error('kdb error: %s', stderr)
      return cb(new Error(stderr))
    }
    return cb(null, trimNewline(stdout), stderr)
  })

const escapeValues = (template, ...values) =>
  template.reduce((acc, part, i) => {
    let val = values[i - 1]
    if (typeof val === 'string') val = `"${val}"`
    return acc + val + part
  })

// TODO: validate user input!

const ls = (path, cb) =>
  safeExec(`kdb ls ${path}`, (err, stdout) => {
    if (err) return cb(err)
    let paths = stdout.split('\n')
    return cb(null, paths)
  }, error)

const get = (path, cb) =>
  safeExec(escapeValues`kdb get ${path}`, cb, error)

const set = (path, value, cb) =>
  safeExec(escapeValues`kdb set ${path} ${value}`, cb, error)

const rm = (path, cb) =>
  safeExec(escapeValues`kdb rm ${path}`, cb, error)

export default { ls, get, set, rm }
