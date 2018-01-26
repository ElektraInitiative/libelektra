/**
 * @file
 *
 * @brief shows errors in a small snackbar on the bottom of the UI
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'
import Snackbar from 'material-ui/Snackbar'

const ErrorSnackbar = ({ error }) => {
  if (error) console.error(error)
  const message = typeof error === 'string' ? error : error.message
  const name = (error && error.name) ? error.name : 'Error'
  if (typeof message === 'string' && message.length > 0) {
    return (
      <Snackbar
        open={!!error}
        message={name + ': ' + message}
      />
    )
  } else {
    return null
  }
}

export default ErrorSnackbar
