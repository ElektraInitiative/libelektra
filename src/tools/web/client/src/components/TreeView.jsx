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
  constructor (...args) {
    super(...args)
    this.state = { selection: [], model: false }
    this.handleSelect = this.handleSelect.bind(this)
    this.handleUpdate = this.handleUpdate.bind(this)
  }

  handleSelect (newSelection, item, ancestors, neighbours) {
    this.setState({ selection: newSelection })
  }

  handleUpdate (model) {
    // TODO: implement this
    this.setState({ model })
  }

  render () {
    const { data } = this.props
    const { selection, model } = this.state
    return (
      <ExplorerView
        model={model || data}
        category="children"
        name="name"
        selection={selection}
        updateModel={this.handleUpdate}
        onSelect={this.handleSelect}
      />
    )
  }
}

TreeView.propTypes = {
  data: PropTypes.arrayOf(PropTypes.object),
}
