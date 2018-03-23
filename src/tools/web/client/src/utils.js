export const toElektraBool = (val) =>
  val ? '1' : '0'

export const fromElektraBool = (val) =>
  (val === '1') ? true : false

export const RANGE_REGEX = /(-?[0-9]+)-(-?[0-9]+)/

export const isNumberType = (type) => {
  switch (type) {
    case 'short':
    case 'unsigned_short':
    case 'long':
    case 'unsigned_long':
    case 'long_long':
    case 'unsigned_long_long':
    case 'float':
    case 'double':
      return true

    default:
      return false
  }
}

export const VISIBILITY_LEVELS = {
  critical: 7,
  important: 6,
  user: 5,
  advanced: 4,
  developer: 3,
  debug: 2,
  disabled: 1,
}

export const visibility = (name) => VISIBILITY_LEVELS[name]
