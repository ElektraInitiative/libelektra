/**
 * @file
 *
 * @brief radio buttons for tree items
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'

import { RadioButton, RadioButtonGroup } from 'material-ui/RadioButton'

const RadioButtons = ({ value, meta, options, onChange }) => (
    <RadioButtonGroup
      defaultSelected={value}
      onChange={(evt, value) => onChange(value)}
      style={{ display: 'inline-block' }}
    >
        {options.map(option =>
            <RadioButton
              value={option}
              label={option}
              style={{ display: 'inline-block', width: 'auto', paddingRight: 32 }}
            />
        )}
    </RadioButtonGroup>
)

export default RadioButtons
