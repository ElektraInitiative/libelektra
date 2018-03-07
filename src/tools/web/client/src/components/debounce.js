/**
 * @file
 *
 * @brief utility higher-order component to debounce a handler function
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from 'react'

export default function debounce (WrappedComponent, {
  timeout = 1000, handlerFn = 'onChange'
} = {}) {
  return class extends React.Component {
    constructor (...args) {
      super(...args)
      this.state = { timeoutFn: false }
    }

    handleChange = (evt, _, val) => {
      const { timeoutFn } = this.state
      const { onChange, onDebounced } = this.props

      const value = val || evt.target.value

      onChange(value)
      if (timeoutFn) clearTimeout(timeoutFn)

      this.setState({
        timeoutFn: setTimeout(() => {
          this.setState({ timeoutFn: false })
          if (typeof onDebounced === 'function') onDebounced(value)
        }, timeout)
      })
    }

    render () {
      const injectedProps = {
        [handlerFn]: this.handleChange,
        onDebounced: null,
      }
      return <WrappedComponent {...this.props} {...injectedProps} />
    }
  }
}
