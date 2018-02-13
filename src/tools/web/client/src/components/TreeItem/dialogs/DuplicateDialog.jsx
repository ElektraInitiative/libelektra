/**
 * @file
 *
 * @brief dialog to duplicate a key
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from 'react'

import Dialog from 'material-ui/Dialog'
import FlatButton from 'material-ui/FlatButton'
import TextField from 'material-ui/TextField'

export default class DuplicateDialog extends Component {
  constructor (...args) {
    super(...args)
    const [ props ] = args
    this.state = {
      name: (props && props.item && props.item.name)
        ? props.item.name + 'Copy'
        : '',
    }
  }

  handleClose = () => {
    const { onClose } = this.props
    this.setState({ name: '' })
    onClose()
  }

  render () {
    const { item, open, onDuplicate } = this.props
    const { path } = item
    const { name } = this.state

    const newPathParts = path.split('/')
    newPathParts.pop()
    const newPath = newPathParts.join('/') + '/' + name

    const actions = [
      <FlatButton
        label="Cancel"
        onTouchTap={this.handleClose}
      />,
      <FlatButton
        label="Duplicate"
        primary={true}
        onTouchTap={() => {
          onDuplicate(path, newPath)
          this.handleClose()
        }}
      />,
    ]

    return (
        <Dialog
          actions={actions}
          modal={false}
          open={open}
          onRequestClose={this.handleClose}
        >
            <h1>Duplicating <b>{path}</b> key</h1>
            <div style={{ display: 'block' }}>
                <TextField
                  ref="nameField"
                  floatingLabelText="name of copy"
                  floatingLabelFixed={true}
                  hintText="e.g. keyNameCopy"
                  onChange={evt => this.setState({ name: evt.target.value })}
                  value={name}
                />
            </div>
        </Dialog>
    )
  }
}
