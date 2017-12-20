/**
 * @file
 *
 * @brief toggle button for tree items
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'

import Checkbox from 'material-ui/Checkbox'

export default class ToggleButton extends React.Component {
  constructor (props, ...args) {
    super(props, ...args)
    this.state = { checked: props.value === '1' }
  }

  handleCheck = (evt, checked) => {
    const { onChange } = this.props
    this.setState({ checked })
    onChange(checked ? '1' : '0')
  }

  render () {
    const { value, meta } = this.props
    return (
        <Checkbox
          label={meta && meta.description}
          checked={this.state.checked}
          onCheck={this.handleCheck}
          style={{ display: 'inline-block', width: 'auto' }}
        />
    )
  }
}
