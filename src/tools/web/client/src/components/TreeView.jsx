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
    this.state = { selection: [], unfolded: [] }
  }

  refresh = () => {
    const { data, getKey, instanceId } = this.props
    const { unfolded } = this.state
    const user = data.find(d => d.path === 'user')
    const allUnfolded = [ user, ...unfolded ]
    return Promise.all(
      allUnfolded.map(this.refreshItem)
    )
  }

  refreshItem = (item) => {
    const { getKey, instanceId } = this.props
    const mainPromise = getKey(instanceId, item.path)
    if (Array.isArray(item.children)) {
      return Promise.all([
        mainPromise,
        ...item.children.map(
          child => getKey(instanceId, child.path)
        ),
      ])
    } else {
      return mainPromise
    }
  }

  handleSelect = (newSelection, item, ancestors, neighbours) => {
    this.refreshItem(item)
    this.setState({ selection: newSelection })
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

  waitForData = () => {
    const { data } = this.props
    const user = Array.isArray(data) && data.find(d => d.path === 'user')
    if (!user) {
      this.timeout = setTimeout(this.waitForData, 100)
    } else {
      this.refreshItem(user)
    }
  }

  componentDidMount () {
    this.waitForData()
  }

  render () {
    const { data } = this.props
    const { selection } = this.state
    const tree = this
    const strategies = {
      click: [ "select", function (item) {
        const newUnfolded = this.state.get().unfolded.filter(i => i.path !== item.path)
        if (newUnfolded.length === this.state.get().unfolded.length) {
          newUnfolded.push(item)
        }
        this.state.set({ unfolded: newUnfolded })
        tree.setState({ unfolded: newUnfolded })
      } ],
      fold: [ function (item) {
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
      />
    )
  }
}
