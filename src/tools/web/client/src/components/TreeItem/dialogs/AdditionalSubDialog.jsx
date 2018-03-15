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

const DebouncedTextField = debounce(TextField)

export default class AdditionalSubDialog extends Component {
  render () {
    const { handleEdit, getMeta, getSaved } = this.props

    return (
        <div style={{ display: 'block' }}>
          TODO
        </div>
    )
  }
}
