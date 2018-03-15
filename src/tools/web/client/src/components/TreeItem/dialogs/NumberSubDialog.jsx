/**
 * @file
 *
 * @brief sub-dialog to modify number metadata of keys
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from 'react'

import TextField from 'material-ui/TextField'

import SavedIcon from '../SavedIcon.jsx'
import { RANGE_REGEX } from '../../../utils'
import debounce from '../../debounce'

const DebouncedTextField = debounce(TextField)

export default class NumberSubDialog extends Component {
  constructor (...args) {
    super(...args)
    this.state = { value: false, error: false }
  }

  validateNumber = (str) => {
    try {
      const ranges = str.split(',')
      return ranges.reduce((res, range) => {
        if (res) return res
        return !RANGE_REGEX.test(range)
      }, false)
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
              floatingLabelText="range"
              floatingLabelFixed={true}
              hintText="e.g. 1-10 or -1-4,6-10"
              value={val}
              errorText={this.state.error}
              onChange={value => this.setState({ value })}
              onDebounced={currentValue => {
                const validationError = this.validateNumber(currentValue)
                if (validationError) {
                  return this.setState({ error: validationError })
                } else {
                  this.setState({ error: false })
                }
                onChange(currentValue)
              }}
            />
            <SavedIcon saved={saved} />
            <p>
              should be formatted like this: <code>from-to</code>,
              multiple ranges are separated by a comma
            </p>
        </div>
    )
  }
}
