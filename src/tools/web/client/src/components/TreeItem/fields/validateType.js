const INTEGER_TYPES = [
  'short', 'unsigned_short', 'long', 'unsigned_long', 'long_long',
  'unsigned_long_long',
]

const FLOAT_TYPES = [ 'float', 'double' ]

const isNumber = (value) => !isNaN(value)

const elektraEnumToJSON = (val) => {
  const convertedVal = val.replace(/'/g, '"')
  if (val.charAt(0) !== '[') return '[' + convertedVal + ']'
  else return convertedVal
}

const validateType = (metadata, value) => {
  if (!metadata) return false // no metadata, no validation
  const type = metadata['check/type'] || 'any'

  if (FLOAT_TYPES.includes(type)) {
    if (!isNumber(value)) {
      return 'invalid number, float expected'
    }
  }

  if (INTEGER_TYPES.includes(type)) {
    const i = Number(value)
    if (!Number.isInteger(i)) {
      return 'invalid number, integer expected'
    }

    if (type === 'short' && !(i >= -32768 && i <= 32767)) {
      return 'invalid number, short (integer between -32768 and 32767) expected'
    } else if (type === 'unsigned_short' && !(i >= 0 && i <= 65535)) {
      return 'invalid number, unsigned short (integer between 0 and 65535) expected'
    } else if (type === 'long' && !(i >= -2147483648 && i <= 2147483647)) {
      return 'invalid number, long (integer between -2147483648 and 2147483647) expected'
    } else if (type === 'unsigned_long' && !(i >= 0 && i <= 4294967295)) {
      return 'invalid number, unsigned long (integer between 0 and 4294967295) expected'
    } else if (type === 'long_long' && !(i >= -9223372036854775808 && i <= 9223372036854775807)) {
      return 'invalid number, long long (integer between -9223372036854775808 and 9223372036854775807) expected'
    } else if (type === 'unsigned_long_long' && !(i >= 0 && i <= 18446744073709551615)) {
      return 'invalid number, unsigned long long (integer between 0 and 18446744073709551615) expected'
    }
  }

  const validationError = metadata['check/validation/message']

  const validationRegex = metadata.hasOwnProperty('check/validation')
    ? new RegExp(metadata['check/validation'])
    : false
  if (validationRegex) {
    if (!validationRegex.test(value)) {
      return validationError ||
        'validation failed for ' + metadata['check/validation']
    }
  }

  const validationEnum = metadata.hasOwnProperty('check/enum')
    ? JSON.parse(elektraEnumToJSON(metadata['check/enum']))
    : false
  if (validationEnum && Array.isArray(validationEnum)) {
    if (!validationEnum.includes(value)) {
      return validationError ||
        'validation failed, value must be one of: ' + validationEnum.join(', ')
    }
  }

  return false
}

export default validateType
