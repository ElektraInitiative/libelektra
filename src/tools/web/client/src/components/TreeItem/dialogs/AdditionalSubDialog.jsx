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
import ContentAddIcon from 'material-ui/svg-icons/content/add'

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
    const keys = Object.keys(meta).filter(k => !HANDLED_METADATA.includes(k))
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

  renderItems = () => {
    const { handleEdit, getMeta, getSaved } = this.props
    const { items } = this.state

    return items.map(item => [
      <DebouncedTextField
        floatingLabelText={item.key}
        floatingLabelFixed={true}
        value={item.value || getMeta(item.key)}
        onChange={this.updateValue(item.key)}
        onDebounced={handleEdit(item.key)}
      />,
      <SavedIcon saved={getSaved(item.key)} />
    ])
  }

  createKey = () => {
    const { handleEdit } = this.props
    const name = prompt("Please enter the key name (e.g. check/condition)")
    if (HANDLED_METADATA.includes(name)) {
      alert(
        "Cannot add metakey '" + name + "' because it is already handled by elektra-web. " +
        "Please use the existing field on the settings page to configure this metakey."
      )
    } else {
      handleEdit(name)('')
    }
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
