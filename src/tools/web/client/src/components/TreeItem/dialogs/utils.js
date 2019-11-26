export const KEY_TYPES = [
  { type: "any", name: "Text (any)" },
  { type: "string", name: "Text (string)" },
  { type: "boolean", name: "Checkbox (boolean)" },
  { type: "enum", name: "Radio (enum)" },
  { type: "short", name: "Number (short)" },
  { type: "unsigned_short", name: "Positive Number (unsigned_short)" },
  { type: "long", name: "Number (long)" },
  { type: "unsigned_long", name: "Positive Number (unsigned_long)" },
  { type: "long_long", name: "Number (long_long)" },
  { type: "unsigned_long_long", name: "Positive Number (unsigned_long_long)" },
  { type: "float", name: "Floating Point Number (float)" },
  { type: "double", name: "Floating Point Number (double)" }
];

export const HANDLED_METADATA = [
  "check/type",
  "check/enum",
  "check/range",
  "restrict/write",
  "restrict/remove",
  "restrict/binary",
  "description",
  "example",
  "default",
  "check/validation",
  "check/validation/message",
  "visibility",
  "binary"
];
