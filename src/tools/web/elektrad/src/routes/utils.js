export const prettyprint = (obj) =>
  JSON.stringify(obj, null, 2)

export const responseCallback = (res) =>
  (err, output) =>
    err
    ? res.status(400).send(prettyprint({ error: err.message }))
    : res.send(prettyprint(output))
