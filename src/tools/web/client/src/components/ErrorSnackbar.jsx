/**
 * @file
 *
 * @brief shows errors in a small notification on the bottom of the UI
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

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
