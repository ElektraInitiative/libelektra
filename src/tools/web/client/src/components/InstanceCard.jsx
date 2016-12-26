import React from 'react'

import { Card, CardHeader, CardText } from 'material-ui/Card'
import FlatButton from 'material-ui/FlatButton'
import TextField from 'material-ui/TextField'
import Checkbox from 'material-ui/Checkbox'

export default class InstanceCard extends React.Component {
  render () {
    const { id, name, host, checked, addingCluster } = this.props
    const { updateInstance, deleteInstance, configureInstance, selectInstance } = this.props // action creators
    const title =
      addingCluster // show checkbox next to title during cluster creation
      ? <Checkbox label={name} checked={checked} onCheck={() => selectInstance(id)} />
      : name

    return (
        <Card style={{ margin: '10px', marginBottom: '25px' }}>
            <CardHeader
              title={title}
              subtitle={host}
              actAsExpander={!addingCluster}
              showExpandableButton={!addingCluster}
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
                  hintText="http://127.0.0.1:33333"
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
