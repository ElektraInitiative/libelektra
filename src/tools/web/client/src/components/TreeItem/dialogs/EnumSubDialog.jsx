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

// TODO: abstract as DebouncedTextField
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
            <TextField
              floatingLabelText="enum"
              floatingLabelFixed={true}
              hintText="e.g. ['option1','option2']"
              value={val}
              errorText={this.state.error}
              onChange={(evt) => {
                if (this.state.timeout) clearTimeout(this.state.timeout)
                const currentValue = evt.target.value
                this.setState({
                  value: currentValue,
                  timeout: setTimeout(() => {
                    const validationError = this.validateEnum(currentValue)
                    if (validationError) {
                      return this.setState({ error: validationError })
                    } else {
                      this.setState({ error: false })
                    }
                    onChange(null, null, currentValue)
                  }, 500),
                })
              }}
            />
            <SavedIcon saved={saved} />
            <p>should be formatted like this: <code>['option1','option2']</code></p>
        </div>
    )
  }
}
