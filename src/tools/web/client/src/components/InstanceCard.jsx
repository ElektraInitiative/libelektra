import React from 'react'

import { Card, CardHeader, CardText } from 'material-ui/Card'
import FlatButton from 'material-ui/FlatButton'
import TextField from 'material-ui/TextField'

export default class InstanceCard extends React.Component {
  render () {
    const { id, name, host } = this.props
    const { updateInstance, deleteInstance, configureInstance } = this.props // action creators
    return (
        <Card style={{ margin: '10px', marginBottom: '25px' }}>
            <CardHeader
              title={name}
              subtitle={host}
              actAsExpander={true}
              showExpandableButton={true}
            />
            <CardText expandable={true}>
                <TextField
                  ref="nameField"
                  floatingLabelText="name"
                  floatingLabelFixed={true}
                  hintText="my webserver"
                  defaultValue={name}
                />
                {' '}
                <TextField
                  ref="hostField"
                  floatingLabelText="host"
                  floatingLabelFixed={true}
                  hintText="http://127.0.0.1:1235"
                  defaultValue={host}
                />
                <div style={{ marginTop: '25px' }}>
                  <FlatButton
                    label="save"
                    primary={true}
                    onTouchTap={() => updateInstance(id, {
                      name: this.refs.nameField.getValue(),
                      host: this.refs.hostField.getValue(),
                    })}
                  />
                  <FlatButton
                    label="configure"
                    onTouchTap={() => configureInstance(id)}
                  />
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
