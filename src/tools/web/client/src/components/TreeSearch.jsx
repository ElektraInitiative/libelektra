/**
 * @file
 *
 * @brief search for the tree view
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'
import debounce from './debounce'

const SearchInput = ({ value, onChange, state }) => (
  <input
    type="search"
    class={'search ' + state}
    placeholder="Search keys..."
    value={value}
    onChange={onChange}
  />
)
const DebouncedSearchInput = debounce(SearchInput)

export default class TreeSearch extends React.Component {
  constructor (...args) {
    super(...args)
    this.state = { value: '' }
  }

  handleChange = (value) => {
    this.setState({ value })
  }

  handleFind = (value) => {
    const { instanceId, findKey } = this.props
    findKey(instanceId, value)
  }

  render () {
    const { search } = this.props
    const { loading, error } = search
    const { value } = this.state
    const state = value.length > 0
      ? (loading
          ? 'loading'
          : (error ? 'error' : 'success')
        )
      : 'empty'
    return (
      <DebouncedSearchInput
        state={state}
        value={value}
        onChange={this.handleChange}
        onDebounced={this.handleFind}
      />
    )
  }
}
