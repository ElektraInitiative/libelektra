/**
 * @file
 *
 * @brief simple editable text field for tree items
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from 'react'

import TextField from 'material-ui/TextField'

import validateType from './validateType'
import debounce from '../../debounce'

const DebouncedTextField = debounce(TextField)

export default class SimpleTextField extends Component {
  constructor (props) {
    super(props)
    this.state = { value: false, error: false }
  }

  render () {
    const { id, value, meta, onChange } = this.props
    const val = this.state.value === false ? value : this.state.value

    return (
      <div draggable="true" onDragStart={e => e.preventDefault()}>
        <DebouncedTextField
          id={id}
          value={val}
          errorText={this.state.error}
          hintText={meta && meta.example}
          onChange={value => this.setState({ value })}
          onDebounced={currentValue => {
            const validationError = validateType(meta, currentValue)
            if (validationError) {
              return this.setState({ error: validationError })
            } else {
              this.setState({ error: false })
            }
            onChange(currentValue)
          }}
        />
      </div>
    )
  }
}
