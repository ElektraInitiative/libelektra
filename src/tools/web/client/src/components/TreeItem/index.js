/**
 * @file
 *
 * @brief interactive tree view item to edit configurations of instances
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from 'react'

import ActionDelete from 'material-ui/svg-icons/action/delete'
import ActionBuild from 'material-ui/svg-icons/action/build'
import ContentAdd from 'material-ui/svg-icons/content/add'
import ContentCopy from 'material-ui/svg-icons/content/content-copy'
import ContentEdit from 'material-ui/svg-icons/editor/mode-edit'

import ActionButton from './ActionButton.jsx'
import SavedIcon from './SavedIcon.jsx'
import SimpleTextField from './fields/SimpleTextField.jsx'
import RadioButtons from './fields/RadioButtons.jsx'
import ToggleButton from './fields/ToggleButton.jsx'
import AddDialog from './dialogs/AddDialog.jsx'
import SettingsDialog from './dialogs/SettingsDialog.jsx'
import RemoveDialog from './dialogs/RemoveDialog.jsx'
import DuplicateDialog from './dialogs/DuplicateDialog.jsx'
import EditDialog from './dialogs/EditDialog.jsx'

export default class TreeItem extends Component {
  constructor (...args) {
    super(...args)
    this.state = {
      dialogs: {
        add: false,
        edit: false,
        settings: false,
        remove: false,
      },
      saved: false,
      savedTimeout: false,
    }
  }

  handleOpen = (dialog) => (e) => {
    e.stopPropagation()
    const { dialogs } = this.state
    this.setState({ dialogs: { ...dialogs, [dialog]: true } })
  }

  handleClose = (dialog) => () => {
    const { dialogs } = this.state
    this.setState({ dialogs: { ...dialogs, [dialog]: false } })
  }

  handleDelete = (path) => {
    const { instanceId, item, deleteKey, sendNotification } = this.props
    deleteKey(instanceId, path)
      .then(() => {
        if (Array.isArray(item.children) && item.children.length > 0) {
          return Promise.all(item.children.map(
            child => deleteKey(instanceId, child.path)
          ))
        }
      })
      .then(() => sendNotification('successfully deleted key: ' + path))
  }

  handleAdd = (path, addKeyName, addKeyValue) => {
    const { instanceId, setKey, sendNotification } = this.props
    const fullPath = path + '/' + addKeyName
    setKey(instanceId, fullPath, addKeyValue).then(() =>
      sendNotification('successfully created key: ' + fullPath)
    )
  }

  handleDuplicate = (from, to) => {
    const { instanceId, copyKey, sendNotification } = this.props
    copyKey(instanceId, from, to).then(() =>
      sendNotification('successfully duplicated key: ' + from + ' -> ' + to)
    )
  }

  handleEdit = (value) => {
    const { savedTimeout } = this.state
    const { instanceId, setKey, item } = this.props
    const { path } = item
    return setKey(instanceId, path, value)
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

  renderSpecialValue = (id, { value, meta }) => {
    if (meta['check/enum']) {
      try {
        const options = JSON.parse(meta['check/enum'].replace(/'/g, '"'))
        return (
            <RadioButtons id={id} value={value} meta={meta} options={options} onChange={this.handleEdit} />
        )
      } catch (err) {
        console.warn('invalid enum type:', meta['check/enum'])
      }
    }

    if (meta['check/type']) {
      if (meta['check/type'] === 'boolean') {
        return (
            <ToggleButton id={id} value={value} meta={meta} onChange={this.handleEdit} />
        )
      }
    }
  }

  renderValue = (id, { value, meta }) => {
    if (meta) {
      const special = this.renderSpecialValue(id, { value, meta })
      if (special) return special
    }

    // fallback
    return (
      <SimpleTextField id={id} value={value} meta={meta} onChange={this.handleEdit} />
    )
  }

  render () {
    const { data, item, instanceId, setMetaKey, deleteMetaKey } = this.props

    const rootLevel = (item && item.path)
      ? !item.path.includes('/')
      : false

    const titleStyle = { marginTop: -3 }

    const meta = data && data.meta
    const isCheckbox = meta && meta['check/type'] && meta['check/type'] === 'boolean'
    const valueVisible = data && !item.children

    return (
        <a style={{ display: 'flex', alignItems: 'center' }}>
            {valueVisible
              ? (
                  <span style={{ display: 'flex', alignItems: 'center', height: 48 }}>
                      <b style={titleStyle}>{item.name + ': '}</b>
                      <span style={{ marginLeft: 6 }}>{this.renderValue(item.path, data)}</span>
                  </span>
                )
              : <b style={titleStyle}>{item.name}</b>
            }
            <span className="actions">
                <SavedIcon saved={this.state.saved} />
                <ActionButton icon={<ContentAdd />} onClick={this.handleOpen('add')} tooltip="create sub-key" />
                {!rootLevel && !valueVisible &&
                  <ActionButton icon={<ContentEdit />} onClick={this.handleOpen('edit')} tooltip="edit value" />
                }
                {!rootLevel &&
                  <ActionButton icon={<ContentCopy />} onClick={this.handleOpen('duplicate')} tooltip="duplicate key" />
                }
                {!rootLevel &&
                  <ActionButton icon={<ActionBuild />} onClick={this.handleOpen('settings')} size={13} tooltip="configure metadata" />
                }
                {!rootLevel &&
                  <ActionButton icon={<ActionDelete />} onClick={this.handleOpen('remove')} tooltip="delete key" />
                }
                <i>
                  {!isCheckbox && meta && meta.description}
                </i>
            </span>
            <AddDialog
              item={item}
              open={this.state.dialogs.add}
              onAdd={this.handleAdd}
              onClose={this.handleClose('add')}
            />
            <EditDialog
              item={item}
              value={data && data.value}
              open={this.state.dialogs.edit}
              onEdit={this.handleEdit}
              onClose={this.handleClose('edit')}
            />
            <DuplicateDialog
              item={item}
              open={this.state.dialogs.duplicate}
              onDuplicate={this.handleDuplicate}
              onClose={this.handleClose('duplicate')}
            />
            <SettingsDialog
              item={item}
              meta={data && data.meta}
              data={data && data.value}
              open={this.state.dialogs.settings}
              setMeta={(key, value) => setMetaKey(instanceId, item.path, key, value)}
              deleteMeta={key => deleteMetaKey(instanceId, item.path, key)}
              onClose={this.handleClose('settings')}
            />
            <RemoveDialog
              item={item}
              open={this.state.dialogs.remove}
              onDelete={this.handleDelete}
              onClose={this.handleClose('remove')}
            />
        </a>
    )
  }
}
