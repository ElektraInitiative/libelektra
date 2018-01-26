/**
 * @file
 *
 * @brief dialog to create a new key
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from 'react'

import Dialog from 'material-ui/Dialog'
import FlatButton from 'material-ui/FlatButton'
import TextField from 'material-ui/TextField'

export default class AddDialog extends Component {
  constructor (...args) {
    super(...args)
    this.state = {
      name: '',
      value: '',
    }
  }

  handleClose = () => {
    const { onClose } = this.props
    this.setState({ name: '', value: '' })
    onClose()
  }

  render () {
    const { item, open, onAdd } = this.props
    const { path } = item
    const { name, value } = this.state

    const actions = [
      <FlatButton
        label="Cancel"
        onTouchTap={this.handleClose}
      />,
      <FlatButton
        label="Create"
        primary={true}
        onTouchTap={() => {
          onAdd(path, name, value)
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
            <h1>Creating new key at <b>{path}</b></h1>
            <div style={{ display: 'block' }}>
                <TextField
                  ref="nameField"
                  floatingLabelText="name"
                  floatingLabelFixed={true}
                  hintText="e.g. keyName"
                  onChange={evt => this.setState({ name: evt.target.value })}
                  value={name}
                />
            </div>
            <div style={{ display: 'block', marginTop: 8 }}>
                <TextField
                  ref="valueField"
                  floatingLabelText="value"
                  floatingLabelFixed={true}
                  hintText="e.g. hello world"
                  onChange={evt => this.setState({ value: evt.target.value })}
                  value={value}
                />
            </div>
        </Dialog>
    )
  }
}
