// adapted from https://github.com/chenglou/react-treeview
import React, { PropTypes } from 'react'

const TreeView = React.createClass({
  propTypes: {
    collapsed: PropTypes.bool,
    defaultCollapsed: PropTypes.bool,
    nodeLabel: PropTypes.node.isRequired,
    className: PropTypes.string,
    itemClassName: PropTypes.string,
  },

  getInitialState() {
    return {collapsed: this.props.defaultCollapsed}
  },

  handleClick(...args) {
    this.setState({collapsed: !this.state.collapsed})
    if (this.props.onClick) {
      this.props.onClick(...args)
    }
  },

  render() {
    const {
      collapsed = this.state.collapsed,
      defaultCollapsed,
      className = '',
      itemClassName = '',
      nodeLabel,
      valueField,
      children,
      ...rest,
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
          <span className={fullItemClassName} onClick={children && this.handleClick}>
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
  },
})

export default TreeView
