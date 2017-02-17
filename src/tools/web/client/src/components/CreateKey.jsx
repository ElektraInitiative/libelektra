/**
 * @file
 *
 * @brief this component is used to create a new key on the configuration page
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

import React from 'react'

import TextField from 'material-ui/TextField'
import RaisedButton from 'material-ui/RaisedButton'

export default class CreateKey extends React.Component {
  constructor (props) {
    super(props)
    this.state = { path: '', value: '' }
  }

  render () {
    const { setKey } = this.props

    return (
      <div style={{ marginLeft: 10 }}>
        Create new key:
        <TextField
          hintText="some/path"
          value={this.state.path}
          onChange={(evt) => this.setState({ path: evt.target.value })}
        />
        <TextField
          hintText="value"
          value={this.state.value}
          onChange={(evt) => this.setState({ value: evt.target.value })}
        />
        <RaisedButton
          label="create key"
          onTouchTap={() => setKey(this.state.path, this.state.value)}
        />
      </div>
    )
  }
}
