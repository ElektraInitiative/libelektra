/**
 * @file
 *
 * @brief shows notifications in a small snackbar on the bottom of the UI
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'
import Snackbar from 'material-ui/Snackbar'

const NotificationSnackbar = ({ message }) => {
  return (
      <Snackbar open={!!message} message={message} autoHideDuration={4500} />
  )
}

export default NotificationSnackbar
