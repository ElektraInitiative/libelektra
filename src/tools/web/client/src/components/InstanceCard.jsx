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
import SelectField from 'material-ui/SelectField'
import MenuItem from 'material-ui/MenuItem'

import { VISIBILITY_LEVELS } from '../utils'

export default class InstanceCard extends React.Component {
  constructor (props) {
    super(props)
    this.state = { visibility: props.visibility }
  }

  handleCreate = () => {
    const { id, updateInstance, sendNotification } = this.props
    updateInstance(id, {
      name: this.refs.nameField.getValue(),
      host: this.refs.hostField.getValue(),
      description: this.refs.descriptionField.getValue(),
      visibility: this.state.visibility,
    })
      .then(() => sendNotification('Instance updated successfully.'))
  }

  handleDelete = () => {
    const { id, deleteInstance, sendNotification } = this.props
    if (window.confirm('Are you sure that you want to delete this instance?')) {
      deleteInstance(id)
        .then(() => sendNotification('Instance deleted successfully.'))
    }
  }

  render () {
    const { id, name, host, description } = this.props
    const { visibility } = this.state

    return (
        <Card style={{ margin: '10px', marginBottom: '25px' }}>
            <CardHeader
              title={
                  <span style={{ fontSize: 24, lineHeight: '30px' }}>{name}</span>
              }
              subtitle={
                <span>
                  {description ? description + ' — ' : ''}
                  host: <span style={{ opacity: 0.7 }}>{host}</span>
                  &nbsp;— visibility: <span style={{ opacity: 0.7 }}>{visibility}</span>
                </span>
              }
              actAsExpander={true}
              showExpandableButton={true}
            />
            <CardText expandable={true}>
                <div style={{ display: 'flex' }}>
                    <div style={{ flex: 1 }}>
                        <TextField
                          ref="nameField"
                          floatingLabelText="name*"
                          floatingLabelFixed={true}
                          hintText="e.g. my webserver"
                          defaultValue={name}
                          disabled={id === 'my'}
                          onKeyPress={e => {
                            if (e.key === 'Enter') {
                              this.handleCreate()
                            }
                          }}
                        />
                    </div>
                    <div style={{ flex: 1 }}>
                        <TextField
                          ref="hostField"
                          floatingLabelText="host*"
                          floatingLabelFixed={true}
                          defaultValue={host}
                          disabled={id === 'my'}
                          onKeyPress={e => {
                            if (e.key === 'Enter') {
                              this.handleCreate()
                            }
                          }}
                        />
                    </div>
                </div>
                <div style={{ display: 'flex', marginTop: 4 }}>
                    <div style={{ flex: 1 }}>
                        <i>* required</i>
                    </div>
                    <div style={{ flex: 1 }}>
                        <i>If elektrad is running on the same machine, host is: </i>
                        <code>http://127.0.0.1:33333</code>
                    </div>
                </div>
                <div style={{ display: 'flex', marginTop: 16 }}>
                  <div style={{ flex: 1 }}>
                      <TextField
                        ref="descriptionField"
                        floatingLabelText="description"
                        floatingLabelFixed={true}
                        defaultValue={description}
                        disabled={id === 'my'}
                        onKeyPress={e => {
                          if (e.key === 'Enter') {
                            this.handleCreate()
                          }
                        }}
                      />
                  </div>
                  <div style={{ flex: 1 }}>
                      <SelectField
                        ref="visibilityField"
                        floatingLabelText="visibility"
                        floatingLabelFixed={true}
                        onChange={(e, _, val) => this.setState({ visibility: val })}
                        value={visibility}
                        disabled={id === 'my'}
                      >
                          {Object.keys(VISIBILITY_LEVELS).map(lvl =>
                            <MenuItem key={lvl} value={lvl} primaryText={lvl} />
                          )}
                      </SelectField>
                  </div>
                </div>
                <div style={{ marginTop: 32 }}>
                  <FlatButton
                    label="save"
                    primary={true}
                    onTouchTap={this.handleCreate}
                    disabled={id === 'my'}
                  />
                  <Link to={'/instances/' + id}>
                      <FlatButton label="configure" />
                  </Link>
                  <FlatButton
                    label="delete"
                    secondary={true}
                    onTouchTap={this.handleDelete}
                    disabled={id === 'my'}
                  />
                </div>
            </CardText>
        </Card>
    )
  }
}
