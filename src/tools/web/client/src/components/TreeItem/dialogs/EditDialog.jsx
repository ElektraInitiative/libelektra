/**
 * @file
 *
 * @brief dialog to modify the value of keys with sub-keys
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from 'react'

import FlatButton from 'material-ui/FlatButton'
import FocusTrapDialog from './FocusTrapDialog.jsx'

export default class EditDialog extends Component {
  render () {
    const { item, open, field, onClose } = this.props
    const { path } = item

    const actions = [
      <FlatButton
        label="Done"
        primary={true}
        onClick={onClose}
        onKeyPress={e => {
          if (e.key === 'Enter') {
            onClose()
          }
        }}
      />,
    ]

    return (
        <FocusTrapDialog
          actions={actions}
          modal={false}
          open={open}
          paused={true}
          onRequestClose={onClose}
        >
            <h1>Value of <b>{path}</b></h1>
            <div style={{ display: 'block' }} tabIndex="0">
                {field}
            </div>
        </FocusTrapDialog>
    )
  }
}
