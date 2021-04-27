/**
 * @file
 *
 * @brief utility functions used in multiple actions
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// create a promise handler that:
//  * emits the first action type (REQUESTED) when the promise is started
//  * emits the second action type (RESOLVED) when everything works well
//  * emits the third action type (REJECTED) when an error happened
export const thunkCreator = (action) => {
  const { types, promise, ...rest } = action;
  const [REQUESTED, RESOLVED, REJECTED] = types;

  return (dispatch) => {
    dispatch({ ...rest, type: REQUESTED });

    return promise
      .then((result) => {
        const error =
          (result && result.error) ||
          (result && result.result && result.result.error);
        return error
          ? dispatch({ ...rest, type: REJECTED, error }) // `error` is only returned in error messages
          : dispatch({ ...rest, type: RESOLVED, result }); // otherwise, dispatch result
      })
      .catch((error) => dispatch({ ...rest, type: REJECTED, error }));
  };
};

// encode kdb path with encodeURIComponent
export const encodePath = (path) =>
  path.split("/").map(encodeURIComponent).join("/");

export const parseJSONResponse = (response) => {
  return response.text().then((text) => {
    try {
      return JSON.parse(text);
    } catch (err) {
      throw new Error("invalid response from server");
    }
  });
};
