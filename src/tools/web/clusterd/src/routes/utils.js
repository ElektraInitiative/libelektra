export const prettyprint = (obj) =>
  JSON.stringify(obj, null, 2)

export const successResponse = (res, output) =>
  res.send(prettyprint(output))

export const errorResponse = (res, err) =>
  res.status(400).send(prettyprint({ error: err.message }))
