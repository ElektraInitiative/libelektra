/**
 * @file
 *
 * @brief interactive tree view item to edit configurations of instances
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'

import TextField from 'material-ui/TextField'
import IconButton from 'material-ui/IconButton'
import ActionDelete from 'material-ui/svg-icons/action/delete'
import ContentAdd from 'material-ui/svg-icons/content/add'

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

  const titleStyle = { marginTop: -3 }

  // TODO: allow editing value of nested items
  const main =
    (data && data.value && !(item.children && item.children.length > 0))
      ? (
          <span>
              <b style={titleStyle}>{item.name + ': '}</b>
              <span style={{ marginLeft: 6 }}>{renderValue(data)}</span>
          </span>
        )
      : <b style={titleStyle}>{item.name}</b>

  const deleteAction = (
      <IconButton
        style={{ width: 16, height: 16, paddingTop: '1px' }}
        iconStyle={{ width: 14, height: 14 }}
        onTouchTap={() => { /* TODO */ console.log('deleted', item)}}
      >
          <ActionDelete />
      </IconButton>
  )

  const addAction = (
      <IconButton
        style={{ width: 16, height: 16, paddingTop: '1px' }}
        iconStyle={{ width: 14, height: 14 }}
        onTouchTap={() => { /* TODO */ console.log('creating', item)}}
      >
          <ContentAdd />
      </IconButton>
  )

  return (
      <a style={{ display: 'flex', alignItems: 'center' }}>
          {main} <span className="actions">{addAction} {deleteAction}</span>
      </a>
  )
}

export default TreeItem
