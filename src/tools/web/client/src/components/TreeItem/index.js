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
import ActionDelete from 'material-ui/svg-icons/action/delete'
import ContentAdd from 'material-ui/svg-icons/content/add'
import Dialog from 'material-ui/Dialog'
import FlatButton from 'material-ui/FlatButton'

export default class TreeItem extends Component {
  constructor (...args) {
    super(...args)
    this.state = {
      addDialog: false,
      deleteDialog: false,
    }
  }

  handleOpenAdd = () => {
    this.setState({ addDialog: true })
  }

  handleCloseAdd = () => {
    this.setState({ addDialog: false })
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

  handleAdd = () => {
    // TODO
  }

  // TODO: render various input fields here
  renderValue = ({ value, meta }) => {
    if (meta && meta.hasOwnProperty('check/type')) {
      switch (meta['check/type']) {
        // TODO
      }
    }

    // fallback
    return (
      <TextField value={value} />
    )
  }

  renderAddDialog = () => {
    return // TODO
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
        onClick={() => this.handleDelete(path)}
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

    return (
        <a style={{ display: 'flex', alignItems: 'center' }}>
            {main} <span className="actions">{addAction} {deleteAction}</span>
            {this.renderAddDialog(item)}
            {this.renderDeleteDialog(item)}
        </a>
    )
  }
}
