/**
 * @file
 *
 * @brief icon button for a tree item action
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'

import IconButton from 'material-ui/IconButton'

const ActionButton = ({ icon, tooltip, onClick, size = 14 }) => (
    <IconButton
      style={{ width: 16, height: 16, paddingTop: 1 }}
      iconStyle={{ width: 14, height: 14 }}
      onClick={onClick}
      tooltip={tooltip}
    >
        {icon}
    </IconButton>
)

export default ActionButton
