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
import SelectField from 'material-ui/SelectField'
import MenuItem from 'material-ui/MenuItem'
import ActionBuild from 'material-ui/svg-icons/action/build'

import { VISIBILITY_LEVELS } from '../../../utils'
import { KEY_TYPES } from './utils'

export default class AddDialog extends Component {
  constructor (props, ...args) {
    super(props, ...args)
    this.state = {
      name: '',
      value: '',
      type: 'any',
      visibility: props.instanceVisibility || 'user',
      error: false,
    }
  }

  generateArrayKey = (length) => {
    const numberStr = String(length)
    const prefix = '_'.repeat(numberStr.length - 1)
    return '#' + prefix + length
  }

  componentWillReceiveProps (nextProps) {
    if (this.state.name.length === 0 && nextProps.arrayKeyLength) {
      this.setState({ name: this.generateArrayKey(nextProps.arrayKeyLength) })
    }
  }

  handleClose = () => {
    const { onClose } = this.props
    this.setState({
      name: '', value: '', type: 'any',
      visibility: props.instanceVisibility || 'user',
      error: false,
    })
    onClose()
  }

  handleCreate = () => {
    const { item, onAdd, setMetaByPath } = this.props
    const { path } = item
    const { name, value, type } = this.state
    onAdd(path, name, value)
    if (type !== 'any') {
      setMetaByPath(path + '/' + name, 'check/type', type)
    }
    if (visibility !== 'user') {
      setMetaByPath(path + '/' + name, 'visibility', visibility)
    }
    this.handleClose()
  }

  render () {
    const { item, open, renderField, arrayKeyLength } = this.props
    const { path } = item
    const { name, value, type, error, visibility } = this.state

    const nameEmpty = !name || name.trim().length <= 0

    const actions = [
      <FlatButton
        label="Cancel"
        onTouchTap={this.handleClose}
      />,
      <FlatButton
        label="Create"
        primary={true}
        onTouchTap={this.handleCreate}
        disabled={nameEmpty || error}
      />,
    ]

    return (
        <Dialog
          actions={actions}
          modal={false}
          open={open}
          onRequestClose={this.handleClose}
        >
            <h1>Creating new {arrayKeyLength ? 'array ' : ''}key at <b>{path}</b></h1>
            <div style={{ display: 'flex', alignItems: 'center' }}>
              <div style={{ flex: 'initial' }}>
                <TextField
                  ref="nameField"
                  floatingLabelText="name"
                  floatingLabelFixed={true}
                  hintText="e.g. keyName"
                  onChange={evt => this.setState({ name: evt.target.value })}
                  value={name}
                  onKeyPress={e => {
                    if (!nameEmpty && !error && e.key === 'Enter') {
                      this.handleCreate()
                    }
                  }}
                />
              </div>
              <div style={{ flex: 'initial', marginLeft: 48, marginTop: 12 }}>
                <i>Hint: create a #0 sub-key in a key without children to turn it into an array</i>
              </div>
            </div>
            <div style={{ display: 'block', marginTop: 8 }}>
                <SelectField
                  floatingLabelText="type"
                  floatingLabelFixed={true}
                  onChange={(e, _, val) => this.setState({ type: val })}
                  value={type}
                >
                    {KEY_TYPES.map(({ type, name }) =>
                      <MenuItem key={type} value={type} primaryText={name} />
                    )}
                </SelectField>
            </div>
            <div style={{ display: 'block', marginTop: 8 }}>
              <SelectField
                floatingLabelText="visibility"
                floatingLabelFixed={true}
                onChange={(e, _, val) => this.setState({ visibility: val })}
                value={visibility}
              >
                  {Object.keys(VISIBILITY_LEVELS).map(lvl =>
                    <MenuItem key={lvl} value={lvl} primaryText={lvl} />
                  )}
              </SelectField>
            </div>
            <div style={{ display: 'block', marginTop: 8 }}>
                {type !== 'enum' && renderField({
                  value,
                  meta: { 'check/type': type },
                  debounce: false,
                  onChange: (value) => this.setState({ value }),
                  onKeyPress: e => {
                    if (!nameEmpty && !error && e.key === 'Enter') {
                      this.handleCreate()
                    }
                  },
                  onError: err => this.setState({ error: err }),
                  label: 'value',
                })}
                {type === 'enum' && (
                  <div style={{ display: 'block', marginTop: 16, color: 'rgba(0, 0, 0, 0.5)' }}>
                      <b style={{ fontSize: '1.1em' }}>Please note:</b><br />
                      You can only define options after the key is created.<br />
                      Please create the key, then
                      <i style={{ paddingLeft: 6, paddingRight: 8 }}>
                        <ActionBuild style={{ width: 14, height: 14, marginRight: 4, color: 'rgba(0, 0, 0, 0.5)' }} />
                        configure metadata
                      </i>
                      to define options.
                  </div>
                )}
            </div>
        </Dialog>
    )
  }
}
