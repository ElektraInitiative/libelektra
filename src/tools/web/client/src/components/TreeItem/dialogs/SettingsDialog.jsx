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
import SelectField from 'material-ui/SelectField'
import MenuItem from 'material-ui/MenuItem'
import Checkbox from 'material-ui/Checkbox'

import SavedIcon from '../SavedIcon.jsx'
import EnumSubDialog from './EnumSubDialog.jsx'

import debounce from '../../debounce'
import { toElektraBool, fromElektraBool } from '../../../utils'

const DebouncedTextField = debounce(TextField)

const IMMEDIATE = 'IMMEDIATE'
const DEBOUNCED = 'DEBOUNCED'

export default class SettingsDialog extends Component {
  constructor (...args) {
    super(...args)
    this.state = {}
  }

  handleEdit = (key, debounced = false) => (val) => {
    const isEnum = key === 'check/type' && val === 'enum'

    const { meta, data, setMeta } = this.props

    // changing from enum
    if (key === 'check/type' && !isEnum && meta && meta['check/enum']) {
      const { deleteMeta } = this.props
      deleteMeta('check/enum')
    }

    // changing to enum
    if (key === 'check/type' && isEnum) {
      setMeta('check/enum', this.getMeta('check/enum', '[\'' + data + '\']'))
    }

    const value = isEnum
      ? 'string' // special case: enum is also a string
      : val

    if (!debounced || debounced === IMMEDIATE) {
      // set value of field
      this.setState({ [key]: { ...this.state[key], value: isEnum ? 'enum' : value } })
    }

    if (!debounced || debounced === DEBOUNCED) {
      // persist value to kdb and show notification
      const { timeout } = this.state[key] || {}
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
  }

  getMeta (key, fallback) {
    const stateVal = this.state[key] && this.state[key].value

    const { meta } = this.props
    const val = meta // meta exists
      ? ((key === 'check/type') && meta['check/enum']) // is enum?
        ? 'enum'
        : meta[key] // is not enum
      : false // does not exist

    return stateVal === undefined
      ? val || fallback
      : stateVal
  }

  getSaved (key) {
    return this.state[key] && this.state[key].saved
  }

  renderEnum () {
    return (
        <EnumSubDialog
          onChange={this.handleEdit('check/enum')}
          value={this.getMeta('check/enum', '')}
          saved={this.getSaved('check/enum')}
        />
    )
  }

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
            <div style={{ display: 'block', marginTop: 32 }}>
                <Checkbox
                  checked={fromElektraBool(this.getMeta('readonly', false))}
                  onCheck={(e, val) => this.handleEdit('readonly')(toElektraBool(val))}
                  label="read only / disabled"
                />
                <SavedIcon saved={this.getSaved('readonly')} />
            </div>
            <div style={{ display: 'flex' }}>
                <div style={{ flex: 1 }}>
                    <DebouncedTextField
                      floatingLabelText="description"
                      floatingLabelFixed={true}
                      hintText="e.g. username of the account"
                      onChange={this.handleEdit('description', IMMEDIATE)}
                      onDebounced={this.handleEdit('description', DEBOUNCED)}
                      value={this.getMeta('description', '')}
                    />
                    <SavedIcon saved={this.getSaved('description')} />
                </div>
                <div style={{ flex: 1 }}>
                    <DebouncedTextField
                      floatingLabelText="example"
                      floatingLabelFixed={true}
                      hintText="e.g. hitchhiker42"
                      onChange={this.handleEdit('example', IMMEDIATE)}
                      onDebounced={this.handleEdit('example', DEBOUNCED)}
                      value={this.getMeta('example', '')}
                    />
                    <SavedIcon saved={this.getSaved('example')} />
                </div>
            </div>
            <div style={{ display: 'flex' }}>
                <div style={{ flex: 1 }}>
                    <DebouncedTextField
                      floatingLabelText="validation regex"
                      floatingLabelFixed={true}
                      hintText="e.g. ^[a-zA-Z0-9]+$"
                      onChange={this.handleEdit('check/validation', IMMEDIATE)}
                      onDebounced={this.handleEdit('check/validation', DEBOUNCED)}
                      value={this.getMeta('check/validation', '')}
                    />
                    <SavedIcon saved={this.getSaved('check/validation')} />
                </div>
                <div style={{ flex: 1 }}>
                    <DebouncedTextField
                      floatingLabelText="validation error message"
                      floatingLabelFixed={true}
                      hintText="e.g. invalid username"
                      onChange={this.handleEdit('check/validation/message', IMMEDIATE)}
                      onDebounced={this.handleEdit('check/validation/message', DEBOUNCED)}
                      value={this.getMeta('check/validation/message', '')}
                    />
                    <SavedIcon saved={this.getSaved('check/validation/message')} />
                </div>
            </div>
            <div style={{ display: 'block', marginTop: 8 }}>
                <SelectField
                  floatingLabelText="type"
                  floatingLabelFixed={true}
                  onChange={(e, _, val) => this.handleEdit('check/type')(val)}
                  value={this.getMeta('check/type', 'string')}
                >
                    <MenuItem value="string" primaryText="Text (string)" />
                    <MenuItem value="boolean" primaryText="Checkbox (boolean)" />
                    <MenuItem value="enum" primaryText="Radio (enum)" />
                    <MenuItem value="short" primaryText="Number (short)" />
                    <MenuItem value="unsigned_short" primaryText="Positive Number (unsigned_short)" />
                    <MenuItem value="long" primaryText="Number (long)" />
                    <MenuItem value="unsigned_long" primaryText="Positive Number (unsigned_long)" />
                    <MenuItem value="long_long" primaryText="Number (long_long)" />
                    <MenuItem value="unsigned_long_long" primaryText="Positive Number (unsigned_long_long)" />
                </SelectField>
                <SavedIcon saved={this.getSaved('check/type')} style={{ paddingBottom: 16 }} />
            </div>
            {this.getMeta('check/enum', false) ? this.renderEnum() : null}
        </Dialog>
    )
  }
}
