/* components/ClusterCard.jsx
this is the representation of a cluster in the main overview
*/

import React from 'react'

import { Card, CardHeader, CardText } from 'material-ui/Card'
import FlatButton from 'material-ui/FlatButton'
import TextField from 'material-ui/TextField'
import HardwareDeviceHub from 'material-ui/svg-icons/hardware/device-hub'

export default class ClusterCard extends React.Component {
  render () {
    const { id, name, instances } = this.props
    const instanceAmt = instances.length
    const { updateCluster, deleteCluster, configureCluster } = this.props // action creators
    const title = ( // show small cluster icon next to title
      <span>
        <HardwareDeviceHub style={{width: '12px', height: '12px'}} />{' '}{name}
      </span>
    )
    return (
        <Card style={{ margin: '10px', marginBottom: '25px' }}>
            <CardHeader
              title={title}
              subtitle={`${instanceAmt} instance${instanceAmt != 1 ? 's' : ''}`}
              actAsExpander={true}
              showExpandableButton={true}
            />
            <CardText expandable={true}>
                <TextField
                  ref="nameField"
                  floatingLabelText="name"
                  floatingLabelFixed={true}
                  hintText="webservers"
                  defaultValue={name}
                />
                <div style={{ marginTop: '25px' }}>
                  <FlatButton
                    label="save"
                    primary={true}
                    onTouchTap={() => updateCluster(id, {
                      name: this.refs.nameField.getValue(),
                    })}
                  />
                  <FlatButton
                    label="configure"
                    onTouchTap={() => configureCluster(id)}
                  />
                  <FlatButton
                    label="delete"
                    secondary={true}
                    onTouchTap={() => deleteCluster(id)}
                  />
                </div>
            </CardText>
        </Card>
    )
  }
}
