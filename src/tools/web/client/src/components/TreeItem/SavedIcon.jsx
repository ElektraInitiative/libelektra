/**
 * @file
 *
 * @brief small icon to notify the user that data was saved
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'

import ActionDone from 'material-ui/svg-icons/action/done'

const SavedIcon = ({ saved, style }) => {
  const savedIconBaseStyle = {
    width: 16,
    height: 16,
    paddingTop: 1,
    paddingLeft: 4,
    color: '#00BCD4',
    transition: 'opacity 0.5s',
  }

  const savedIconActiveStyle = saved
    ? { opacity: 1 }
    : { opacity: 0 }

  const savedIconStyle = { ...savedIconBaseStyle, ...savedIconActiveStyle, ...style }

  return (
      <ActionDone className="savedIcon" style={savedIconStyle} />
  )
}

export default SavedIcon
