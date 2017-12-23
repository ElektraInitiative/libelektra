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
import TextField from 'material-ui/TextField'

import SavedIcon from '../SavedIcon.jsx'

export default class SettingsDialog extends Component {
  constructor (props, ...args) {
    super(props, ...args)
    const { meta } = props
    this.state = {
      description: { value: (meta && meta.description) ? meta.description : '' }
    }
  }

  handleEdit = key => evt => {
    // set value of field
    const { value } = evt.target
    this.setState({ [key]: { ...this.state[key], value } })

    // persist value to kdb and show notification
    const { setMeta } = this.props
    const { timeout } = this.state[key]
    setMeta(key, value)
      .then(() => {
        if (timeout) clearTimeout(timeout)
        this.setState({ [key]: {
          ...this.state[key],
          saved: true,
          timeout: setTimeout(() => {
            this.setState({ [key]: { ...this.state[key], saved: false } })
          }, 1500),
        } })
      })
  }

  render () {
    const { item, meta, open, onClose } = this.props
    const { path } = item
    console.log('meta', meta)

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
            <TextField
              floatingLabelText="description"
              floatingLabelFixed={true}
              hintText="placeholder for the text field"
              onChange={this.handleEdit('description')}
              value={this.state.description.value}
            />
            <SavedIcon saved={this.state.description && this.state.description.saved} />
        </Dialog>
    )
  }
}
