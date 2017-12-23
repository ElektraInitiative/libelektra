/**
 * @file
 *
 * @brief dialog to modify metadata of keys
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from 'react'

import Dialog from 'material-ui/Dialog'
import FlatButton from 'material-ui/FlatButton'

export default class SettingsDialog extends Component {
  render () {
    const { item, open, onClose } = this.props
    const { path } = item

    const actions = [
      <FlatButton
        label="Done"
        primary={true}
        onTouchTap={onClose}
      />,
    ]

    return (
        <Dialog
          actions={actions}
          modal={false}
          open={open}
          onRequestClose={onClose}
        >
            <h1>Settings for <b>{path}</b></h1>
        </Dialog>
    )
  }
}
