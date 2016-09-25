export const prettyprint = (obj) =>
  JSON.stringify(obj, null, 2)

export const responseCallback = (res) =>
  (output) =>
    res.send(prettyprint(output))
