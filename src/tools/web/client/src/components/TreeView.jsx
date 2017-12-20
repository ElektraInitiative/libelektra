/**
 * @file
 *
 * @brief interactive tree view to edit configurations of instances
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'
import PropTypes from 'prop-types'
import { ExplorerView } from '@bosket/react'
import { array } from '@bosket/tools'

import TreeItem from '../containers/ConnectedTreeItem'

import '../css/treeview.css'

export default class TreeView extends React.Component {
  constructor (props, ...args) {
    super(props, ...args)
    this.state = { selection: [] }
  }

  handleSelect = (newSelection, item, ancestors, neighbours) => {
    this.setState({ selection: newSelection })
    if (Array.isArray(item.children)) {
      const { getKey, instanceId } = this.props
      getKey(instanceId, item.path)
      Promise.all(
        item.children.map(
          child => getKey(instanceId, child.path)
        )
      )
    }
  }

  handleDrop = (target, evt, inputs) => {
    const { instanceId, moveKey } = this.props
    const { selection } = inputs

    const moveOps = selection.map(
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

  render () {
    const { data } = this.props
    const { selection } = this.state
    const strategies = {
      click: [ "select", function (item) {
        const newUnfolded = this.state.get().unfolded.filter(p => p !== item.path)
        if (newUnfolded.length === this.state.get().unfolded.length) {
          newUnfolded.push(item.path)
        }
        this.state.set({ unfolded: newUnfolded })
      } ],
      fold: [ function (item) {
        return (item && item.path === 'user')
          ? false // always unfold `user`
          : !array(this.state.get().unfolded).contains(item.path)
      } ]
    }
    return (
      <ExplorerView
        dragndrop={{ drop: this.handleDrop }}
        model={data}
        category="children"
        name="name"
        selection={selection}
        strategies={strategies}
        updateModel={this.handleUpdate}
        onSelect={this.handleSelect}
        display={this.renderItem}
      />
    )
  }
}
