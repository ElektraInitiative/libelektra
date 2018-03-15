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
    case 'any':
      return true

    default:
      return false
  }
}
