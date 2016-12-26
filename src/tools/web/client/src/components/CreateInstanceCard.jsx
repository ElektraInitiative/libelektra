import React from 'react'

import { Card, CardHeader, CardText } from 'material-ui/Card'
import FlatButton from 'material-ui/FlatButton'
import TextField from 'material-ui/TextField'

export default class CreateInstanceCard extends React.Component {
  constructor (props) {
    super(props)
    this.state = {
      name: '',
      host: '',
    }
  }

  resetValues () {
    this.setState({
      name: '',
      host: '',
    })
  }

  render () {
    const { createInstance, unaddInstance } = this.props // action creators
    return (
        <Card style={{ margin: '10px', marginBottom: '25px' }}>
            <CardHeader
              title="add new instance"
              subtitle="please enter a host and a friendly name"
            />
            <CardText>
                <TextField
                  ref="nameField"
                  floatingLabelText="name"
                  floatingLabelFixed={true}
                  hintText="my webserver"
                  onChange={(evt) => this.setState({ name: evt.target.value })}
                  value={this.state.name}
                />
                {' '}
                <TextField
                  ref="hostField"
                  floatingLabelText="host"
                  floatingLabelFixed={true}
                  hintText="http://127.0.0.1:33333"
                  onChange={(evt) => this.setState({ host: evt.target.value })}
                  value={this.state.host}
                />
                <div style={{ marginTop: '25px' }}>
                  <FlatButton
                    label="add"
                    primary={true}
                    onTouchTap={() => {
                      createInstance({
                        name: this.state.name,
                        host: this.state.host,
                      })
                      this.resetValues()
                    }}
                  />
                  <FlatButton
                    label="cancel"
                    onTouchTap={() => {
                      unaddInstance()
                      this.resetValues()
                    }}
                  />
                </div>
            </CardText>
        </Card>
    )
  }
}
