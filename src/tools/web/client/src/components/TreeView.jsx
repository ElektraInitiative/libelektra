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
    if (newSelection.includes(item)) {
      const { getKey, instanceId } = this.props
      getKey(instanceId, item.path)
      Promise.all(
        item.children.map(
          child => getKey(instanceId, child.path)
        )
      ).then(
        () => this.setState({ selection: newSelection })
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
    return (data && data.value)
      ? item.name + ': ' + data.value
      : item.name
  }

  render () {
    const { selection, model } = this.state
    return (
      <ExplorerView
        model={model}
        category="children"
        name="name"
        selection={selection}
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
