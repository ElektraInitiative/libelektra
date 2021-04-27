import { isNumberType, RANGE_REGEX } from "../../../utils";

const INTEGER_TYPES = [
  "short",
  "unsigned_short",
  "long",
  "unsigned_long",
  "long_long",
  "unsigned_long_long",
];

const FLOAT_TYPES = ["float", "double"];

const isNumber = (value) => !isNaN(value);

const getMinMax = (first, second) => {
  if (second < first) {
    return [second, first];
  }
  return [first, second];
};

export const validateRange = (rangeStr, num) => {
  const ranges = rangeStr.split(",");
  let msg = "invalid number, value between ";

  const valid = ranges.reduce((res, range, i) => {
    if (res) return res;
    const [, first, second] = range.match(RANGE_REGEX);
    const [min, max] = getMinMax(Number(first), Number(second));
    if (num >= min && num <= max) {
      return true;
    }
    if (i > 0) {
      msg += " or ";
    }
    msg += min + " and " + max;
    return res;
  }, false);

  if (!valid) {
    return msg + " expected";
  }
};

const validateType = (metadata, value) => {
  if (!metadata) return false; // no metadata, no validation
  const type = metadata["check/type"] || "any";

  if (type === "boolean") {
    if (value !== "0" && value !== "1") {
      return "invalid boolean, 0 or 1 expected";
    }
  }

  if (type === "enum") {
    let done = false;
    let valid = false;
    let i = 0;
    let values = [];
    while (!done) {
      const v = metadata[`check/enum/#${i}`];
      if (!v) {
        done = true;
        break;
      }
      values.push(v);
      if (value === v) {
        done = true;
        valid = true;
        break;
      }
      i++;
    }
    if (!valid) {
      return "invalid value, expected one of: " + values.join(", ");
    }
  }

  if (FLOAT_TYPES.includes(type)) {
    if (!isNumber(value)) {
      return "invalid number, float expected";
    }
  }

  if (INTEGER_TYPES.includes(type)) {
    const i = Number(value);
    if (!Number.isInteger(i)) {
      return "invalid number, integer expected";
    }

    if (type === "short" && !(i >= -32768 && i <= 32767)) {
      return "invalid number, short (integer between -32768 and 32767) expected";
    } else if (type === "unsigned_short" && !(i >= 0 && i <= 65535)) {
      return "invalid number, unsigned short (integer between 0 and 65535) expected";
    } else if (type === "long" && !(i >= -2147483648 && i <= 2147483647)) {
      return "invalid number, long (integer between -2147483648 and 2147483647) expected";
    } else if (type === "unsigned_long" && !(i >= 0 && i <= 4294967295)) {
      return "invalid number, unsigned long (integer between 0 and 4294967295) expected";
    } else if (
      type === "long_long" &&
      !(i >= -9223372036854775808 && i <= 9223372036854775807)
    ) {
      return "invalid number, long long (integer between -9223372036854775808 and 9223372036854775807) expected";
    } else if (
      type === "unsigned_long_long" &&
      !(i >= 0 && i <= 18446744073709551615)
    ) {
      return "invalid number, unsigned long long (integer between 0 and 18446744073709551615) expected";
    }
  }

  if (isNumberType(type)) {
    const i = Number(value);
    const range = metadata["check/range"];
    if (range) {
      const validationError = validateRange(range, i);
      if (validationError) {
        return validationError;
      }
    }
  } else {
    const validationErrorMessage = metadata["check/validation/message"];

    const validationRegex = metadata.hasOwnProperty("check/validation")
      ? new RegExp(metadata["check/validation"])
      : false;
    if (validationRegex) {
      if (!validationRegex.test(value)) {
        return (
          validationErrorMessage ||
          "validation failed for " + metadata["check/validation"]
        );
      }
    }
  }

  return false;
};

export default validateType;
