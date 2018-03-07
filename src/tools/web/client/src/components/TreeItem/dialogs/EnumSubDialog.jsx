/**
 * @file
 *
 * @brief sub-dialog to modify enum (radio button) metadata of keys
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from 'react'

import TextField from 'material-ui/TextField'

import SavedIcon from '../SavedIcon.jsx'

import debounce from '../../debounce'

const DebouncedTextField = debounce(TextField)

export default class EnumSubDialog extends Component {
  constructor (...args) {
    super(...args)
    this.state = { value: false, error: false }
  }

  validateEnum = (str) => {
    try {
      JSON.parse(str.replace(/'/g, '"'))
    } catch (err) {
      return 'invalid format'
    }
  }

  render () {
    const { value, saved, onChange } = this.props
    const val = this.state.value === false ? value : this.state.value

    return (
        <div style={{ display: 'block', marginTop: 8 }}>
            <DebouncedTextField
              floatingLabelText="enum"
              floatingLabelFixed={true}
              hintText="e.g. ['option1','option2']"
              value={val}
              errorText={this.state.error}
              onChange={value => this.setState({ value })}
              onDebounced={currentValue => {
                const validationError = this.validateEnum(currentValue)
                if (validationError) {
                  return this.setState({ error: validationError })
                } else {
                  this.setState({ error: false })
                }
                onChange(currentValue)
              }}
            />
            <SavedIcon saved={saved} />
            <p>should be formatted like this: <code>['option1','option2']</code></p>
        </div>
    )
  }
}
