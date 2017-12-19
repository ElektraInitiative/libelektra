/**
 * @file
 *
 * @brief interactive tree view item to edit configurations of instances
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from 'react'

import TextField from 'material-ui/TextField'
import IconButton from 'material-ui/IconButton'
import ActionDone from 'material-ui/svg-icons/action/done'
import ActionDelete from 'material-ui/svg-icons/action/delete'
import ContentAdd from 'material-ui/svg-icons/content/add'
import Dialog from 'material-ui/Dialog'
import FlatButton from 'material-ui/FlatButton'

import SimpleTextField from './SimpleTextField.jsx'
import RadioButtons from './RadioButtons.jsx'

export default class TreeItem extends Component {
  constructor (...args) {
    super(...args)
    this.state = {
      addDialog: false,
      deleteDialog: false,
      addKeyName: '',
      addKeyValue: '',
      saved: false,
      savedTimeout: false,
    }
  }

  handleOpenAdd = () => {
    this.setState({ addDialog: true })
  }

  handleCloseAdd = () => {
    this.setState({ addDialog: false, addKeyName: '', addKeyValue: '' })
  }

  handleOpenDelete = () => {
    this.setState({ deleteDialog: true })
  }

  handleCloseDelete = () => {
    this.setState({ deleteDialog: false })
  }

  handleDelete = (path) => {
    const { instanceId, deleteKey, sendNotification } = this.props
    deleteKey(instanceId, path)
      .then(() => sendNotification('successfully deleted key: ' + path))
    this.handleCloseDelete()
  }

  handleAdd = (path) => {
    const { instanceId, setKey, sendNotification } = this.props
    const { addKeyName, addKeyValue } = this.state
    const fullPath = path + '/' + addKeyName
    setKey(instanceId, fullPath, addKeyValue)
      .then(() => sendNotification('successfully created key: ' + fullPath))
    this.handleCloseAdd()
  }

  handleEdit = (value) => {
    const { savedTimeout } = this.state
    const { instanceId, setKey, sendNotification, item } = this.props
    const { path } = item
    setKey(instanceId, path, value)
      .then(() => {
        if (savedTimeout) clearTimeout(savedTimeout)
        this.setState({
          saved: true,
          savedTimeout: setTimeout(() => {
            this.setState({ saved: false })
          }, 1500),
        })
      })
  }

  renderSpecialValue = ({ value, meta }) => {
    if (meta.hasOwnProperty('check/enum')) {
      try {
        const options = JSON.parse(meta['check/enum'].replace(/\'/g, '"'))
        return (
            <RadioButtons value={value} meta={meta} options={options} onChange={this.handleEdit} />
        )
      } catch (err) {
        console.error('invalid enum type:', meta['check/enum'])
        return false
      }
    }
  }

  // TODO: render various input fields here
  renderValue = ({ value, meta }) => {
    if (meta) {
      const special = this.renderSpecialValue({ value, meta })
      if (special) return special
    }

    // fallback
    return (
      <SimpleTextField value={value} meta={meta} onChange={this.handleEdit} />
    )
  }

  renderAddDialog = ({ path }) => {
    const { addDialog } = this.state
    const actions = [
      <FlatButton
        label="Cancel"
        onTouchTap={this.handleCloseAdd}
      />,
      <FlatButton
        label="Create"
        primary={true}
        onTouchTap={() => this.handleAdd(path)}
      />,
    ]
    return (
        <Dialog
          actions={actions}
          modal={false}
          open={addDialog}
          onRequestClose={this.handleCloseAdd}
        >
            <h1>Creating new key at <b>{path}</b></h1>
            <div style={{ display: 'block' }}>
                <TextField
                  ref="nameField"
                  floatingLabelText="name"
                  floatingLabelFixed={true}
                  hintText="e.g. keyName"
                  onChange={(evt) => this.setState({ addKeyName: evt.target.value })}
                  value={this.state.addKeyName}
                />
            </div>
            <div style={{ display: 'block', marginTop: 8 }}>
                <TextField
                  ref="valueField"
                  floatingLabelText="value"
                  floatingLabelFixed={true}
                  hintText="e.g. hello world"
                  onChange={(evt) => this.setState({ addKeyValue: evt.target.value })}
                  value={this.state.addKeyValue}
                />
            </div>
        </Dialog>
    )
  }

  renderDeleteDialog = ({ path }) => {
    const { deleteDialog } = this.state
    const actions = [
      <FlatButton
        label="Cancel"
        onTouchTap={this.handleCloseDelete}
      />,
      <FlatButton
        label="Delete"
        secondary={true}
        onTouchTap={() => this.handleDelete(path)}
      />,
    ]
    return (
        <Dialog
          actions={actions}
          modal={false}
          open={deleteDialog}
          onRequestClose={this.handleCloseDelete}
        >
            <h1>Delete key <b>{path}</b>?</h1>
            <p>
                Are you sure you want to delete the "<b>{path}</b>" key,
                including all its sub-keys?
            </p>
        </Dialog>
    )
  }

  render () {
    const { data, item, inputs } = this.props
    const { addDialog, deleteDialog } = this.state

    // console.log('item', { data, item, inputs })

    const titleStyle = { marginTop: -3 }

    // TODO: allow editing value of nested items
    const main =
      (data && data.value && !(item.children && item.children.length > 0))
        ? (
            <span>
                <b style={titleStyle}>{item.name + ': '}</b>
                <span style={{ marginLeft: 6 }}>{this.renderValue(data)}</span>
            </span>
          )
        : <b style={titleStyle}>{item.name}</b>

    const addAction = (
        <IconButton
          style={{ width: 16, height: 16, paddingTop: '1px' }}
          iconStyle={{ width: 14, height: 14 }}
          onTouchTap={this.handleOpenAdd}
        >
            <ContentAdd />
        </IconButton>
    )

    const deleteAction = (
        <IconButton
          style={{ width: 16, height: 16, paddingTop: '1px' }}
          iconStyle={{ width: 14, height: 14 }}
          onTouchTap={this.handleOpenDelete}
        >
            <ActionDelete />
        </IconButton>
    )

    const savedIconBaseStyle = {
      width: 16,
      height: 16,
      paddingTop: 1,
      paddingLeft: 4,
      color: '#00BCD4',
      transition: 'opacity 0.5s',
    }
    const savedIconActiveStyle = this.state.saved
      ? { opacity: 1 }
      : { opacity: 0 }
    const savedIconStyle = { ...savedIconBaseStyle, ...savedIconActiveStyle }

    return (
        <a style={{ display: 'flex', alignItems: 'center' }}>
            {main}
            <span className="actions">
                <ActionDone className="savedIcon" style={savedIconStyle} />
                {addAction}
                {deleteAction}
            </span>
            {this.renderAddDialog(item)}
            {this.renderDeleteDialog(item)}
        </a>
    )
  }
}
