/**
 * @file
 *
 * @brief dialog to confirm removal of keys
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from 'react'

import Dialog from 'material-ui/Dialog'
import FlatButton from 'material-ui/FlatButton'

export default class RemoveDialog extends Component {
  render () {
    const { item, open, onClose, onDelete } = this.props
    const { path } = item

    const actions = [
      <FlatButton
        label="Cancel"
        onTouchTap={onClose}
      />,
      <FlatButton
        label="Delete"
        secondary={true}
        onTouchTap={() => {
          onDelete(path)
          onClose()
        }}
      />,
    ]

    const additionalText =
      (Array.isArray(item.children) && item.children.length > 0)
        ? ', including all its sub-keys?'
        : '?'

    return (
        <Dialog
          actions={actions}
          modal={false}
          open={open}
          onRequestClose={onClose}
        >
            <h1>Delete key <b>{path}</b>?</h1>
            <p>
                Are you sure you want to delete
                the "<b>{path}</b>" key{additionalText}
            </p>
        </Dialog>
    )
  }
}
