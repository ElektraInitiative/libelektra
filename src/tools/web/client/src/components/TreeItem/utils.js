export const parseEnum = (valueFn, i = 0, values = []) => {
  const v = valueFn(i) // get next enum value
  console.log('v', v)
  if (v === false || typeof v === 'undefined') { // we are done
    console.log('done')
    return values
  }
  return parseEnum(valueFn, i + 1, [ ...values, v ])
}
