export const parseEnum = (valueFn, i = 0, values = []) => {
  const v = valueFn(i) // get next enum value
  if (v) {
    return parseEnum(valueFn, i + 1, [ ...values, v ])
  }
  return values
}
