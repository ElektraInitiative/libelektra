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
import NumberSubDialog from './NumberSubDialog.jsx'
import AdditionalMetakeysSubDialog from './AdditionalMetakeysSubDialog.jsx'

import debounce from '../../debounce'
import { toElektraBool, fromElektraBool, isNumberType } from '../../../utils'
import { KEY_TYPES } from './utils'

const DebouncedTextField = debounce(TextField)

const IMMEDIATE = 'IMMEDIATE'
const DEBOUNCED = 'DEBOUNCED'

export default class SettingsDialog extends Component {
  constructor (...args) {
    super(...args)
    this.state = {}
  }

  handleEdit = (key, debounced = false) => (value) => {
    const { meta, data, setMeta } = this.props

    if (!debounced || debounced === IMMEDIATE) {
      // set value of field
      this.setState({ [key]: { ...this.state[key], value } })
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
      ? meta[key]
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
          onChange={i => this.handleEdit(`check/enum/#${i}`)}
          value={i => this.getMeta(`check/enum/#${i}`, '')}
          saved={i => this.getSaved(`check/enum/#${i}`)}
        />
    )
  }

  renderNumber () {
    return (
        <NumberSubDialog
          onChange={this.handleEdit('check/range')}
          value={this.getMeta('check/range', '')}
          saved={this.getSaved('check/range')}
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

    const type = this.getMeta('check/type', 'any')

    return (
        <Dialog
          actions={actions}
          modal={false}
          open={open}
          onRequestClose={onClose}
        >
            <h1>Metadata for <b>{path}</b></h1>
            <div style={{ display: 'block' }}>
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
            <div style={{ display: 'flex' }}>
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
                <div style={{ flex: 1 }}>
                    <DebouncedTextField
                      floatingLabelText="default value"
                      floatingLabelFixed={true}
                      onChange={this.handleEdit('default', IMMEDIATE)}
                      onDebounced={this.handleEdit('default', DEBOUNCED)}
                      value={this.getMeta('default', '')}
                    />
                    <SavedIcon saved={this.getSaved('default')} />
                </div>
            </div>
            <h2 style={{ marginTop: 48 }}>Type</h2>
            <div style={{ display: 'block', color: 'rgba(245, 166, 35, 0.5)' }}>
                <b style={{ fontSize: '1.1em' }}>Please note:</b><br />
                Changing the type might result in loss of data. For example,
                when changing from <code>string</code> to <code>boolean</code>,
                the string value will get overwritten!
            </div>
            <div style={{ display: 'flex' }}>
                <div style={{ flex: 1 }}>
                    <SelectField
                      floatingLabelText="type"
                      floatingLabelFixed={true}
                      onChange={(e, _, val) => this.handleEdit('check/type')(val)}
                      value={type}
                    >
                        {KEY_TYPES.map(({ type, name }) =>
                          <MenuItem key={type} value={type} primaryText={name} />
                        )}
                    </SelectField>
                    <SavedIcon saved={this.getSaved('check/type')} style={{ paddingBottom: 16 }} />
                </div>
                <div style={{ flex: 1, paddingTop: 32 }}>
                    <Checkbox
                      checked={fromElektraBool(this.getMeta('readonly', false))}
                      onCheck={(e, val) => this.handleEdit('readonly')(toElektraBool(val))}
                      label="read only"
                    />
                    <SavedIcon saved={this.getSaved('readonly')} />
                </div>
            </div>
            {this.getMeta('check/type', false) === 'enum' ? this.renderEnum() : null}
            {isNumberType(this.getMeta('check/type', false)) ? this.renderNumber() : null}
            {(type === 'string' || type === 'any') &&
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
            }
            <AdditionalMetakeysSubDialog
              handleEdit={this.handleEdit.bind(this)}
              getMeta={this.getMeta.bind(this)}
              getSaved={this.getSaved.bind(this)}
              meta={this.props.meta}
              deleteMeta={this.props.deleteMeta}
            />
        </Dialog>
    )
  }
}
