/**
 * @file
 *
 * @brief sub-dialog to add and modify additional metadata of keys
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from 'react'

import TextField from 'material-ui/TextField'

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

    return items.map(item =>
      <div>
        <DebouncedTextField
          floatingLabelText={item.key}
          floatingLabelFixed={true}
          value={item.value || getMeta(item.key)}
          onChange={this.updateValue(item.key)}
          onDebounced={handleEdit(item.key)}
        />
        <SavedIcon saved={getSaved(item.key)} />
      </div>
    )
  }

  render () {
    const { items } = this.state

    return (
        <div style={{ display: 'block' }}>
          {(items && Array.isArray(items) && items.length > 0)
            ? this.renderItems()
            : <div style={{ fontSize: '1.1em', color: 'rgba(0, 0, 0, 0.4)' }}>
                No additional metadata defined yet.
              </div>
          }
        </div>
    )
  }
}
