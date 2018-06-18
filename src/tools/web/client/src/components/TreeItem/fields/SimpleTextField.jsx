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
import { fromElektraBool } from '../../../utils'

const DebouncedTextField = debounce(TextField, { timeout: 1500 })

export default class SimpleTextField extends Component {
  constructor (props) {
    super(props)
    this.state = { value: props.value || '', error: false }
  }

  componentWillReceiveProps (nextProps) {
    if (nextProps.value !== this.props.value) {
      this.setState({ value: nextProps.value })
    }
  }

  render () {
    const { id, meta, label, debounce = true, onChange, onError, onKeyPress } = this.props
    const val = this.state.value
    const comp = debounce ? DebouncedTextField : TextField
    const isBinary = meta && meta.hasOwnProperty('binary')

    return (
      <div draggable="true" onDragStart={e => e.preventDefault()}>
        {React.createElement(comp, {
          id,
          value: val || (isBinary ? '(null)' : ''),
          tabIndex: 0,
          className: 'value',
          errorText: this.state.error,
          hintText: (meta && meta.example) ? `e.g. ${meta.example}` : false,
          onChange: debounce
            ? value => this.setState({ value })
            : evt => (evt && evt.target && evt.target.value) ? onChange(evt.target.value) : onChange(''),
          onDebounced: debounce && (currentValue => {
            const validationError = validateType(meta, currentValue)
            if (validationError) {
              if (typeof onError === 'function') onError(validationError)
              return this.setState({ error: validationError })
            } else {
              if (typeof onError === 'function') onError(false)
              this.setState({ error: false })
            }
            onChange(currentValue)
          }),
          disabled: isBinary || fromElektraBool(meta && meta['restrict/write']),
          floatingLabelText: label,
          floatingLabelFixed: !!label,
          onKeyPress: onKeyPress,
        })}
      </div>
    )
  }
}
