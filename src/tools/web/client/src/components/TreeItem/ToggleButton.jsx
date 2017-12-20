/**
 * @file
 *
 * @brief toggle button for tree items
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'

import Checkbox from 'material-ui/Checkbox'

const ToggleButton = ({ value, meta, onChange }) => (
    <Checkbox
      label={meta && meta.description}
      checked={value == 'true'}
      onCheck={(evt, checked) => onChange(String(checked))}
      style={{ display: 'inline-block', width: 'auto' }}
    />
)

export default ToggleButton
