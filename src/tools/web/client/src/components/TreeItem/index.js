/**
 * @file
 *
 * @brief interactive tree view item to edit configurations of instances
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'

import TextField from 'material-ui/TextField'

// TODO: render various input fields here
const renderValue = ({ value, meta }) => {
  if (meta && meta.hasOwnProperty('check/type')) {
    switch (meta['check/type']) {
      // TODO
    }
  }

  // fallback
  return (
    <TextField value={value} />
  )
}

const TreeItem = ({ data, item, inputs }) => {
  // console.log('item', { data, item, inputs })
  return (data && data.value)
    ? (
        <a>
            <b>{item.name + ': '}</b>
            <span style={{ marginLeft: 6 }}>{renderValue(data)}</span>
        </a>
      )
    : <a><b>{item.name}</b></a>
}

export default TreeItem
