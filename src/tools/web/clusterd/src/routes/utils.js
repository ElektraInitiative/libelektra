export const prettyprint = (obj) =>
  JSON.stringify(obj, null, 2)

export const responseCallback = (res) =>
  (err, output) =>
    res.send(prettyprint(output))

export const promiseResponseCallback = (res) =>
  (output) =>
    responseCallback(res)(null, output)
