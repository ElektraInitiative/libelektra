/**
 * @file
 *
 * @brief interactive tree view to edit configurations of instances and clusters
 *
 * adapted from https://github.com/chenglou/react-treeview
 *  - added functionality to make a dynamic tree view that allows editing of tree
 *    nodes with values (via `valueField`)
 *  - adjusted css class names
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { PropTypes } from 'react'

export default class TreeView extends React.Component {
  constructor (props) {
    super(props)
    this.state = {
      collapsed: this.props.defaultCollapsed,
    }
  }

  handleClick (...args) {
    this.setState({ collapsed: !this.state.collapsed })
    if (this.props.onClick) {
      this.props.onClick(...args)
    }
  }

  render () {
    const {
      collapsed = this.state.collapsed,
      className = '',
      itemClassName = '',
      nodeLabel,
      valueField,
      children,
      defaultCollapsed, // eslint-disable-line
      ...rest
    } = this.props

    let arrowClassName = 'tree-view-arrow'
    let containerClassName = 'tree-view-children'
    if (collapsed) {
      arrowClassName += ' tree-view-arrow-collapsed'
      containerClassName += ' tree-view-children-collapsed'
    }

    const arrow =
      children
        ? <div {...rest} className={className + ' ' + arrowClassName} />
        : null

    const fullItemClassName =
      children
        ? 'tree-view-haschildren ' + itemClassName
        : itemClassName

    return (
      <div className="tree-view">
        <div className="tree-view-item">
          <span
            className={fullItemClassName}
            style={{ fontSize: 16 }}
            onClick={(args) => children && this.handleClick(args)}
          >
            {arrow}
            {nodeLabel}
          </span>
          <span>
            {valueField}
          </span>
        </div>
        <div className={containerClassName}>
          {children}
        </div>
      </div>
    )
  }
}

TreeView.propTypes = {
  collapsed: PropTypes.bool,
  defaultCollapsed: PropTypes.bool,
  nodeLabel: PropTypes.node.isRequired,
  className: PropTypes.string,
  itemClassName: PropTypes.string,
  valueField: PropTypes.element,
  children: PropTypes.arrayOf(PropTypes.element),
  onClick: PropTypes.func,
}
