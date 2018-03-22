/**
 * @file
 *
 * @brief section of the settings dialog to modify enum (radio button) metadata
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from 'react'

import TextField from 'material-ui/TextField'

import SavedIcon from '../SavedIcon.jsx'
import debounce from '../../debounce'
import { parseEnum } from '../utils'

const DebouncedTextField = debounce(TextField)

export default class EnumSubDialog extends Component {
  constructor (props, ...args) {
    super(props, ...args)
    this.state = { values: parseEnum(props.value) }
  }

  // keep state in sync with property updates
  componentWillReceiveProps (nextProps) {
    this.setState({ values: parseEnum(nextProps.value) })
  }

  render () {
    return (
        <div style={{ display: 'block' }}>
            {JSON.stringify(this.state.values)}
        </div>
    )
  }
}
