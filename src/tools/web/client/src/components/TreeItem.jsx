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

import TreeView from './TreeView.jsx'

// TODO: debounce onChange requests here, we're calling set every time it changes a little
export default class TreeItem extends React.Component {
  constructor (props) {
    super(props)
    this.state = { value: props.value }
  }

  render () {
    const {
      name, prefix, value, children, allowDelete = true,
      onClick, onChange, onDelete,
    } = this.props

    const val = this.state.value || value

    let textField = val ? ( // if a key has a value, show a textfield
        <span>
            {': '}
            <TextField
              id={`${prefix}_textfield`}
              value={val}
              floatingLabelText={this.state.saved ? 'saved!' : null}
              onChange={(evt) => {
                if (this.state.timeout) clearTimeout(this.state.timeout)
                const currentValue = evt.target.value
                this.setState({
                  value: evt.target.value,
                  timeout: setTimeout(() => {
                    onChange(currentValue)
                    this.setState({ saved: true })
                    setTimeout(() => {
                      this.setState({ saved: false })
                    }, 1000)
                  }, 500),
                })
              }}
            />
        </span>
    ) : null

    let deleteButton = (
      <IconButton
        style={{ width: 16, height: 16, padding: 0 }}
        iconStyle={{ width: 16, height: 16 }}
        onTouchTap={onDelete}
      >
        <ActionDelete />
      </IconButton>
    )

    let valueField = ( // valueField = textField + delete button
      <span>
        {textField}
        {allowDelete && deleteButton}
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
