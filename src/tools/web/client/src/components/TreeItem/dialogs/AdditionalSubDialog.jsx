/**
 * @file
 *
 * @brief sub-dialog to add and modify additional metadata of keys
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from 'react'

import TextField from 'material-ui/TextField'
import FlatButton from 'material-ui/FlatButton'
import IconButton from 'material-ui/IconButton'
import ContentAddIcon from 'material-ui/svg-icons/content/add'
import ActionDeleteIcon from 'material-ui/svg-icons/action/delete'

import SavedIcon from '../SavedIcon.jsx'
import debounce from '../../debounce'
import { HANDLED_METADATA } from './SettingsDialog.jsx'

const DebouncedTextField = debounce(TextField)

export default class AdditionalSubDialog extends Component {
  constructor (props, ...args) {
    super(props, ...args)
    this.state = {
      items: (props && props.meta)
        ? this.parseMetadata(props.meta) || []
        : [],
    }
  }

  componentWillReceiveProps (nextProps) {
    if (nextProps && nextProps.meta) {
      this.setState({ items: this.parseMetadata(nextProps.meta) || [] })
    }
  }

  parseMetadata = (meta) => {
    const keys = Object.keys(meta)
      .filter(k => !HANDLED_METADATA.includes(k))
      .filter(k => meta[k] !== undefined)
    return keys.map(k => {
      return { key: k, value: meta[k] }
    })
  }

  updateValue = (key) => (value) => {
    const { items } = this.state
    this.setState({ items: items.map(item => {
      if (item.key === key) {
        return { key, value }
      }
      return item
    }) })
  }

  deleteItem = (item) => {
    if (window.confirm('Do you really want to delete the \'' + item.key + '\' metakey?')) {
      const { deleteMeta } = this.props
      deleteMeta(item.key) // TODO: this should update the items array
    }
  }

  renderItems = () => {
    const { handleEdit, getMeta, getSaved } = this.props
    const { items } = this.state

    return items.map(item => (
      <span style={{ marginRight: 60 }}>
        <DebouncedTextField
          floatingLabelText={item.key}
          floatingLabelFixed={true}
          value={item.value || getMeta(item.key)}
          onChange={this.updateValue(item.key)}
          onDebounced={handleEdit(item.key)}
        />
        <SavedIcon saved={getSaved(item.key)} />
        <IconButton
          style={{ width: 24, height: 24, padding: 4 }}
          iconStyle={{ width: 16, height: 16 }}
          onClick={() => this.deleteItem(item)}
        >
          <ActionDeleteIcon />
        </IconButton>
      </span>
    ))
  }

  createKey = () => {
    const name = prompt("Please enter the key name (e.g. check/condition)")

    if (!name || !name.length || name.trim().length <= 0) {
      return alert("Empty/invalid metakey name.")
    }

    if (HANDLED_METADATA.includes(name)) {
      return alert(
        "Cannot add metakey '" + name + "' because it is already handled by elektra-web. " +
        "Please use the existing field on the settings page to configure this metakey."
      )
    }

    const { handleEdit } = this.props
    return handleEdit(name)('')
  }

  render () {
    const { items } = this.state
    const renderedItems = (items && Array.isArray(items) && items.length > 0)
      ? this.renderItems()
      : [
        <div style={{ fontSize: '1.1em', color: 'rgba(0, 0, 0, 0.4)', marginTop: 16 }}>
          No additional metadata defined yet.
        </div>
      ]

    return [
      <h2 style={{ marginTop: 48, marginBottom: 0, display: 'block' }}>
        Additional Metadata
        <FlatButton
          label="create new metakey"
          icon={<ContentAddIcon />}
          primary
          style={{ marginLeft: 16 }}
          onClick={this.createKey}
        />
      </h2>,
      ...renderedItems,
    ]
  }
}
