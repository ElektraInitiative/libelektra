import React from "react";

export const toElektraBool = (val) => (val ? "1" : "0");

export const fromElektraBool = (val) => (val === "1" ? true : false);

export const RANGE_REGEX = /([-+]?[0-9]*\.?[0-9]+)-([-+]?[0-9]*\.?[0-9]+)/;
export const HOST_REGEX = /(https?:\/\/[^/]+)(\/.*)?/;
export const ARRAY_KEY_REGEX = /#(_*)([0-9]+)/;

export const prettyPrintArrayIndex = (str) => {
  const match = str.match(ARRAY_KEY_REGEX);
  if (!match) return str;
  const [, prefix, index] = match;
  return (
    <span>
      <span style={{ opacity: 0.4 }}>#</span>
      <span style={{ opacity: 0.3 }}>{prefix}</span>
      <span style={{ fontWeight: "bold" }}>{index}</span>
    </span>
  );
};

export const isNumberType = (type) => {
  switch (type) {
    case "short":
    case "unsigned_short":
    case "long":
    case "unsigned_long":
    case "long_long":
    case "unsigned_long_long":
    case "float":
    case "double":
      return true;

    default:
      return false;
  }
};

export const VISIBILITY_LEVELS = {
  critical: 7,
  important: 6,
  user: 5,
  advanced: 4,
  developer: 3,
  internal: 2,
};

export const visibility = (name) => VISIBILITY_LEVELS[name];
