/**
 * @file
 *
 * @brief a single item in the interactive tree view
 *
 * can have a text field to change the value, it also provides other operations
 * (like deleting keys)
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

import React from 'react'

import TextField from 'material-ui/TextField'
import IconButton from 'material-ui/IconButton'
import ActionDelete from 'material-ui/svg-icons/action/delete'
import ContentCreate from 'material-ui/svg-icons/content/create'

import TreeView from './TreeView.jsx'

// TODO: set min/max appropriately for these types
const INTEGER_TYPES = [
  'short', 'unsigned_short', 'long', 'unsigned_long', 'long_long',
  'unsigned_long_long',
]

const FLOAT_TYPES = [ 'float', 'double' ]

const isNumber = (value) => !isNaN(parseFloat(value)) && isFinite(value)
const isInt = (value) => /(-|\+)?[0-9]+/.test(value)

const validateType = (metadata, value) => {
  if (!metadata) return false // no metadata, no validation
  const type = metadata['check/type'] || 'any'

  if (FLOAT_TYPES.includes(type)) {
    if (!isNumber(value)) {
      return 'invalid number, float expected'
    }
  }

  if (INTEGER_TYPES.includes(type)) {
    if (!isInt(value)) {
      return 'invalid number, integer expected'
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
    ? JSON.parse(metadata['check/enum'].replace(/'/g, '"'))
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
              }, 1000)
            }, 500),
          })
        }}
      />
    )
  }

  render () {
    const {
      name, prefix, value, metadata, children, allowDelete = true,
      onClick, onChange, onDelete,
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
          defaultCollapsed={true}
          onClick={onClick}
        >
            {children}
        </TreeView>
    )
  }
}
