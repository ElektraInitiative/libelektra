/* routes/utils.js
utility functions used in routes
*/

export const prettyprint = (obj) =>
  JSON.stringify(obj, null, 2)

export const successResponse = (res, output) =>
  output
    ? res.type('application/json').send(prettyprint(output))
    : res.status(404).send() // no output -> 404

export const errorResponse = (res, err) => {
  if (process.env.NODE_ENV !== 'production') console.error(err)
  return res.status(400).type('application/json')
            .send(prettyprint({ message: err.message }))
}
