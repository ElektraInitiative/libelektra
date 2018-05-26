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
import FocusTrap from 'focus-trap-react'

export default class RemoveDialog extends Component {
  render () {
    const { item, open, onClose, onDelete } = this.props
    const { path } = item

    const actions = (
      <FocusTrap active={open} focusTrapOptions={{ escapeDeactivates: false, returnFocusOnDeactivate: false }}>
        <FlatButton
          ref={node => this.cancelButton = node}
          label="Cancel"
          onClick={onClose}
          onKeyPress={e => {
            if (e.key === 'Enter') {
              onClose()
            }
          }}
        />
        <FlatButton
          label="Delete"
          secondary={true}
          onClick={() => {
            onDelete(path)
            onClose()
          }}
          onKeyPress={e => {
            if (e.key === 'Enter') {
              onDelete(path)
              onClose()
            }
          }}
        />,
      </FocusTrap>
    )

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
