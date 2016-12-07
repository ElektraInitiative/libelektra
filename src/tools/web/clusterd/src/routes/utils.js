export const prettyprint = (obj) =>
  JSON.stringify(obj, null, 2)

export const successResponse = (res, output) =>
  output
    ? res.type('application/json').send(prettyprint(output))
    : res.status(404).send() // no output -> 404

export const errorResponse = (res, err) =>
  res.status(400).type('application/json')
     .send(prettyprint({ message: err.message }))
