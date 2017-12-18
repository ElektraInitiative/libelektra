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

import TreeItem from './TreeItem'

import '../css/treeview.css'

export default class TreeView extends React.Component {
  constructor (props, ...args) {
    super(props, ...args)
    const { data } = props
    this.state = { selection: [], model: data }
    this.handleSelect = this.handleSelect.bind(this)
    this.handleUpdate = this.handleUpdate.bind(this)
    this.renderItem = this.renderItem.bind(this)
  }

  handleSelect (newSelection, item, ancestors, neighbours) {
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

  handleUpdate (model) {
    // TODO: implement this
    this.setState({ model })
  }

  renderItem (item, inputs) {
    const { kdb } = this.props
    const data = kdb && kdb[item.path]
    return <TreeItem data={data} item={item} inputs={inputs} />
  }

  render () {
    const { selection, model } = this.state
    const strategies = {
      click: [ "select", "toggle-fold" ],
    }
    return (
      <ExplorerView
        model={model}
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

TreeView.propTypes = {
  data: PropTypes.arrayOf(PropTypes.object),
}
