/**
 * @file
 *
 * @brief interactive tree view to edit configurations of instances
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'
import { ExplorerView } from 'bosket-react-fork'

import TreeItem from '../containers/ConnectedTreeItem'

import '../css/treeview.css'

export default class TreeView extends React.Component {
  constructor (props, ...args) {
    super(props, ...args)
    this.state = { selection: [], unfolded: [] }
  }

  refreshPath = (path) => {
    const { getKey, instanceId } = this.props
    return getKey(instanceId, path)
  }

  refreshItem = (item) => {
    const mainPromise = this.refreshPath(item.path)
    if (Array.isArray(item.children)) {
      return Promise.all([
        mainPromise,
        ...item.children.map(
          child => this.refreshPath(child.path)
        ),
      ])
    } else {
      return mainPromise
    }
  }

  handleSelect = (newSelection, item, ancestors, neighbours) => {
    this.setState({ selection: newSelection })
    this.refreshItem(item)
  }

  handleDrop = (target, evt, inputs) => {
    const { instanceId, moveKey } = this.props
    const { selection } = inputs

    selection.map(
      sel => moveKey(instanceId, sel.path, target.path + '/' + sel.name)
    )
    this.setState({ selection: [] })
  }

  renderItem = (item, inputs) => {
    const { kdb, instanceId } = this.props
    const data = kdb && kdb[item.path]
    return (
        <TreeItem
          data={data}
          item={item}
          inputs={inputs}
          instanceId={instanceId}
        />
    )
  }

  handleSearch = input => item => {
    // check name and path first
    const regex = new RegExp(`.*${input}.*`, "gi")
    if (item.name.match(regex) || item.path.match(regex)) {
      return true
    }

    // if available, check data too
    const { kdb } = this.props
    const data = kdb && kdb[item.path]
    if (data && data.value && data.value.match(regex)) {
      return true
    }

    return false
  }

  render () {
    const { data } = this.props
    const { selection } = this.state
    const tree = this
    const strategies = {
      click: [ "select", function unfoldOnSelectionByPath (item) {
        if (!this.isSelected(item)) {
          const newUnfolded = this.state.get().unfolded.filter(i => i.path !== item.path)
          newUnfolded.push(item)
          this.state.set({ unfolded: newUnfolded })
          tree.setState({ unfolded: newUnfolded })
        }
      } ],
      fold: [ function unfoldByPath (item) {
        return (item && item.path === 'user')
          ? false // always unfold `user`
          : !this.state.get().unfolded.find(i => i.path === item.path)
      } ]
    }

    return (
      <ExplorerView
        dragndrop={{ drop: this.handleDrop }}
        model={data}
        category="children"
        name="name"
        search={this.handleSearch}
        selection={selection}
        strategies={strategies}
        updateModel={this.handleUpdate}
        onSelect={this.handleSelect}
        display={this.renderItem}
        transition={{
          transitionName: 'ExplorerViewTransition',
          transitionEnterTimeout: 200,
          transitionLeaveTimeout: 200,
        }}
        openerOpts={{ position: 'left' }}
      />
    )
  }
}
