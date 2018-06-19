/**
 * @file
 *
 * @brief shows errors in a small snackbar on the bottom of the UI
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'
import Snackbar from 'material-ui/Snackbar'
import Dialog from 'material-ui/Dialog'
import FlatButton from 'material-ui/FlatButton'

export default class ErrorSnackbar extends React.Component {
  constructor (...args) {
    super(...args)
    this.state = { dialogOpen: false }
  }

  handleOpen = () => {
    this.setState({ dialogOpen: true })
  }

  handleClose = () => {
    const { dismissError } = this.props
    this.setState({ dialogOpen: false })
    dismissError()
  }

  render () {
    const { error, dismissError } = this.props
    const { dialogOpen } = this.state

    if (error) console.error(error)

    const message = typeof error === 'string' ? error : error.message
    const name = (error && error.name) ? error.name : 'Error'
    if (typeof message !== 'string' || message.length <= 0) {
      return null
    }

    const fullStr = name + ': ' + message

    let str = fullStr
    let action = 'dismiss'
    let onClick = dismissError
    if (str.length > 37) {
      str = str.slice(0, 37) + '...'
      action = 'view & ' + action
      onClick = this.handleOpen
    }

    const actions = [
      <FlatButton
        label="report issue"
        onClick={() => window.open('http://issues.libelektra.org/new', '_blank')}
      />,
      <FlatButton
        label="dismiss"
        primary={true}
        onClick={this.handleClose}
      />,
    ]

    return [
      <Snackbar
        open={!!error}
        message={str}
        onRequestClose={() => {/* do nothing */}}
        action={action}
        onActionClick={onClick}
        className="errorSnackbar"
      />,
      <Dialog
        actions={actions}
        modal={false}
        open={dialogOpen}
        onRequestClose={this.handleClose}
      >
        <h1>{name}</h1>
        <pre style={{ whiteSpace: 'pre-wrap' }}>
          {message}
        </pre>
      </Dialog>
    ]
  }
}
