/**
 * @file
 *
 * @brief shows errors in a small snackbar on the bottom of the UI
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'
import Snackbar from 'material-ui/Snackbar'

const ErrorSnackbar = ({ error, dismissError }) => {
  if (error) console.error(error)
  const message = typeof error === 'string' ? error : error.message
  const name = (error && error.name) ? error.name : 'Error'
  if (typeof message === 'string' && message.length > 0) {
    const fullStr = name + ': ' + message

    let str = fullStr
    let action = 'dismiss'
    let onClick = dismissError
    if (str.length > 37) {
      str = str.slice(0, 37) + '...'
      action = 'view & ' + action
      onClick = () => {
        alert(fullStr)
        dismissError()
      }
    }
    
    return (
      <Snackbar
        open={!!error}
        message={str}
        onRequestClose={() => {/* do nothing */}}
        action={action}
        onActionClick={onClick}
      />
    )
  } else {
    return null
  }
}

export default ErrorSnackbar
