/**
 * @file
 *
 * @brief interactive tree view to edit configurations of instances
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'
import { ExplorerView } from 'bosket-react'
import IconButton from 'material-ui/IconButton'

import TreeItem from '../containers/ConnectedTreeItem'
import { visibility } from '../utils'

import '../css/treeview.css'

const NAMESPACES_ORDER = [ 'spec', 'dir', 'user', 'system' ]

export default class TreeView extends React.Component {
  constructor (props, ...args) {
    super(props, ...args)
    this.state = { selection: [], unfolded: [] }
  }

  refreshPath = (path) => {
    const { getKey, instanceId } = this.props
    return getKey(instanceId, path)
  }

  refreshItem = (item, noRecursive = false) => {
    const mainPromise = this.refreshPath(item.path)
    if (!noRecursive && Array.isArray(item.children)) {
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
    this.refreshItem(item, true)
  }

  handleDrop = (target, evt, inputs) => {
    const { instanceId, moveKey } = this.props
    const { selection } = inputs
    console.log('!! target', target)

    selection.map(
      sel => moveKey(instanceId, sel.path, target.path + '/' + sel.name)
    )
    this.setState({ selection: [] })
  }

  renderItem = (item, inputs) => {
    const { kdb, instanceId, instanceVisibility } = this.props
    const data = kdb && kdb[item.path]

    if (data && data.meta && data.meta['visibility']) {
      const lvl = visibility(data.meta['visibility'])
      if (lvl < visibility(instanceVisibility)) {
        // hide this item
        return false
      }
    }
    return (
        <TreeItem
          data={data}
          item={item}
          inputs={inputs}
          instanceId={instanceId}
          pathExists={(path) => kdb && kdb[path]}
          instanceVisibility={instanceVisibility}
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

  handleSort = (a, b) => {
    if (a.root) { // is a namespace -> special ordering
      const aI = NAMESPACES_ORDER.indexOf(a.name)
      const bI = NAMESPACES_ORDER.indexOf(b.name)
      return aI - bI
    }
    return !a.children === !b.children
      ? a.name.localeCompare(b.name)
      : a.children ? -1 : 1
  }

  createOpener () {
    const tree = this
    const { unfolded } = this.state
    return class Opener extends React.Component {
      onClick = (event) => {
        const { onClick, item } = this.props
        const newUnfolded = unfolded.filter(p => p !== item.path)
        if (newUnfolded.length === unfolded.length) {
          newUnfolded.push(item.path)
        }
        tree.setState({ unfolded: newUnfolded })
        onClick(event)
        event.stopPropagation()
      }

      render () {
        const { onClick, item, children, ...rest } = this.props
        return (
          <span
            {...rest}
            tabIndex="0"
            onClick={this.onClick}
            onKeyPress={e => {
              if (e.key === 'Enter') {
                this.onClick(e)
              }
            }}
          >
            {children}
          </span>
        )
      }
    }
  }

  render () {
    const { data } = this.props
    const { selection, unfolded } = this.state
    const tree = this
    const strategies = {
      click: [ function unfoldOnSelectionByPath (item) {
        if (!this.isSelected(item)) {
          const newUnfolded = unfolded.filter(p => p !== item.path)
          if (newUnfolded.length === unfolded.length) {
            newUnfolded.push(item.path)
            tree.setState({ unfolded: newUnfolded })
          }
        }
        return this.inputs.get().onSelect(item, this.inputs.get().ancestors, this.inputs.get().model)
      } ],
      fold: [ function unfoldByPath (item) {
        return !unfolded.find(p => p === item.path)
      } ]
    }

    return (
      <ExplorerView
        dragndrop={{
          drop: this.handleDrop,
          droppable: true, /* allow dropping to keys without children */
        }}
        model={data}
        category="children"
        name="name"
        opener={this.createOpener()}
        search={this.handleSearch}
        sort={this.handleSort}
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
