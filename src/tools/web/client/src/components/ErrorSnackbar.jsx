import React from 'react'
import Snackbar from 'material-ui/Snackbar'

const ErrorSnackbar = ({ error }) => {
  if (error) console.error(error)
  return (
    <Snackbar
      open={!!error}
      message={(error && error.message) ? error.message : ''}
    />
  )
}

export default ErrorSnackbar
