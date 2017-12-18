/**
 * @file
 *
 * @brief this is the representation of an instance in the main overview
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'

import { Card, CardHeader, CardText } from 'material-ui/Card'
import FlatButton from 'material-ui/FlatButton'
import TextField from 'material-ui/TextField'
import { Link } from 'react-router-dom'

export default class InstanceCard extends React.Component {
  render () {
    const { id, name, host } = this.props
    const { updateInstance, deleteInstance } = this.props // action creators

    return (
        <Card style={{ margin: '10px', marginBottom: '25px' }}>
            <CardHeader
              title={
                  <span style={{ fontSize: 24, lineHeight: '30px' }}>{name}</span>
              }
              subtitle={host}
              actAsExpander={true}
              showExpandableButton={true}
            />
            <CardText expandable={true}>
                <div style={{ display: 'block' }}>
                  <TextField
                    ref="nameField"
                    floatingLabelText="name"
                    floatingLabelFixed={true}
                    hintText="my webserver"
                    defaultValue={name}
                  />
                </div>
                <div style={{ display: 'block', marginTop: 8 }}>
                  <TextField
                    ref="hostField"
                    floatingLabelText="host"
                    floatingLabelFixed={true}
                    hintText="http://127.0.0.1:33333"
                    defaultValue={host}
                  />
                </div>
                <div style={{ marginTop: 32 }}>
                  <FlatButton
                    label="save"
                    primary={true}
                    onTouchTap={() => updateInstance(id, {
                      name: this.refs.nameField.getValue(),
                      host: this.refs.hostField.getValue(),
                    })}
                  />
                  <Link to={'/instances/' + id}>
                      <FlatButton label="configure" />
                  </Link>
                  <FlatButton
                    label="delete"
                    secondary={true}
                    onTouchTap={() => deleteInstance(id)}
                  />
                </div>
            </CardText>
        </Card>
    )
  }
}
