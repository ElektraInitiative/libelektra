export const thunkCreator = (action) => {
  const { types, promise, ...rest } = action
  const [ REQUESTED, RESOLVED, REJECTED ] = types

  return (dispatch) => {
    dispatch({ ...rest, type: REQUESTED })

    return promise
      .then(
        (result) => dispatch({ ...rest, type: RESOLVED, result })
      )
      .catch(
        (error) => dispatch({ ...rest, type: REJECTED, error })
      )
  }
}

export const encodePath = (path) =>
  path.split('/').map(encodeURIComponent).join('/')
