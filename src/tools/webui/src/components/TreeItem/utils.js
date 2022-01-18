export const parseEnum = (valueFn, i = 0, values = []) => {
  const v = valueFn(i); // get next enum value
  if (v === false || typeof v === "undefined") {
    // we are done
    return values;
  }
  return parseEnum(valueFn, i + 1, [...values, v]);
};
