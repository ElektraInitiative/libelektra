/**
 * @file
 *
 * @brief a single item in the interactive tree view
 *
 * can have a text field to change the value, it also provides other operations
 * (like deleting keys)
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'

import TextField from 'material-ui/TextField'
import IconButton from 'material-ui/IconButton'
import ActionDelete from 'material-ui/svg-icons/action/delete'
import ContentCreate from 'material-ui/svg-icons/content/create'

import TreeView from './TreeView.jsx'

const INTEGER_TYPES = [
  'short', 'unsigned_short', 'long', 'unsigned_long', 'long_long',
  'unsigned_long_long',
]

const FLOAT_TYPES = [ 'float', 'double' ]

const isNumber = (value) => !isNaN(value)

const elektraEnumToJSON = (val) => {
  const convertedVal = val.replace(/'/g, '"')
  if (val.charAt(0) !== '[') return '[' + convertedVal + ']'
  else return convertedVal
}

const validateType = (metadata, value) => {
  if (!metadata) return false // no metadata, no validation
  const type = metadata['check/type'] || 'any'

  if (FLOAT_TYPES.includes(type)) {
    if (!isNumber(value)) {
      return 'invalid number, float expected'
    }
  }

  if (INTEGER_TYPES.includes(type)) {
    const i = Number(value)
    if (!Number.isInteger(i)) {
      return 'invalid number, integer expected'
    }

    if (type === 'short' && !(i >= -32768 && i <= 32767)) {
      return 'invalid number, short (integer between -32768 and 32767) expected'
    } else if (type === 'unsigned_short' && !(i >= 0 && i <= 65535)) {
      return 'invalid number, unsigned short (integer between 0 and 65535) expected'
    } else if (type === 'long' && !(i >= -2147483648 && i <= 2147483647)) {
      return 'invalid number, long (integer between -2147483648 and 2147483647) expected'
    } else if (type === 'unsigned_long' && !(i >= 0 && i <= 4294967295)) {
      return 'invalid number, unsigned long (integer between 0 and 4294967295) expected'
    } else if (type === 'long_long' && !(i >= -9223372036854775808 && i <= 9223372036854775807)) {
      return 'invalid number, long long (integer between -9223372036854775808 and 9223372036854775807) expected'
    } else if (type === 'unsigned_long_long' && !(i >= 0 && i <= 18446744073709551615)) {
      return 'invalid number, unsigned long long (integer between 0 and 18446744073709551615) expected'
    }
  }

  const validationError = metadata['check/validation/message']

  const validationRegex = metadata.hasOwnProperty('check/validation')
    ? new RegExp(metadata['check/validation'])
    : false
  if (validationRegex) {
    if (!validationRegex.test(value)) {
      return validationError ||
        'validation failed for ' + metadata['check/validation']
    }
  }

  const validationEnum = metadata.hasOwnProperty('check/enum')
    ? JSON.parse(elektraEnumToJSON(metadata['check/enum']))
    : false
  if (validationEnum && Array.isArray(validationEnum)) {
    if (!validationEnum.includes(value)) {
      return validationError ||
        'validation failed, value must be one of: ' + validationEnum.join(', ')
    }
  }

  return false
}

// TODO: debounce onChange requests here, we're calling set every time it changes a little
export default class TreeItem extends React.Component {
  constructor (props) {
    super(props)
    this.state = { value: false, error: false, expanded: false }
  }

  renderField () {
    const {
      name, prefix, value, metadata, children, allowDelete = true,
      onClick, onChange, onDelete,
    } = this.props

    const val = this.state.value === false ? value : this.state.value

    return (
      <TextField
        id={`${prefix}_textfield`}
        value={val}
        errorText={this.state.error}
        floatingLabelText={this.state.saved ? 'saved!' : null}
        onChange={(evt) => {
          if (this.state.timeout) clearTimeout(this.state.timeout)
          const currentValue = evt.target.value
          this.setState({
            value: currentValue,
            timeout: setTimeout(() => {
              const validationError = validateType(metadata, currentValue)
              if (validationError) {
                return this.setState({ error: validationError })
              } else {
                this.setState({ error: false })
              }
              onChange(currentValue)
              this.setState({ saved: true })
              setTimeout(() => {
                this.setState({ saved: false })
              }, 900)
            }, 500),
          })
        }}
      />
    )
  }

  render () {
    const {
      name, prefix, value, metadata, children, allowDelete = true,
      onClick, onChange, onDelete, defaultCollapsed = true,
    } = this.props

    const val = this.state.value || value

    const { description } = metadata

    let field = (val || this.state.expanded) ? ( // if a key has a value, show a textfield
        <span>
            {': '}
            {this.renderField()}
        </span>
    ) : ( // otherwise, show icon that will expand the textfield
        <span style={{ marginLeft: 4 }}>
          <IconButton
            style={{ width: 12, height: 12, padding: 0 }}
            iconStyle={{ width: 12, height: 12 }}
            onTouchTap={() => this.setState({ expanded: true })}
          >
            <ContentCreate />
          </IconButton>
        </span>
    )

    let deleteButton = (
      <IconButton
        style={{ width: 16, height: 16, padding: 0 }}
        iconStyle={{ width: 16, height: 16 }}
        onTouchTap={onDelete}
      >
        <ActionDelete />
      </IconButton>
    )

    let descriptionBox = description && (
      <i style={{ color: 'gray', marginLeft: 10 }}>
        {description}
      </i>
    )

    let valueField = ( // valueField = field + delete button
      <span>
        {field}
        {allowDelete && deleteButton}
        {descriptionBox}
      </span>
    )

    return (
        <TreeView
          nodeLabel={name}
          valueField={valueField}
          defaultCollapsed={defaultCollapsed}
          onClick={onClick}
        >
            {children}
        </TreeView>
    )
  }
}
