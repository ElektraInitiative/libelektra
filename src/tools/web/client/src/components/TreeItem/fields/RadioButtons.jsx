/**
 * @file
 *
 * @brief radio buttons for tree items
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'

import { RadioButton, RadioButtonGroup } from 'material-ui/RadioButton'

const RadioButtons = ({ id, value, meta, options, onChange }) => (
    <RadioButtonGroup
      id={id}
      defaultSelected={value}
      onChange={(evt, value) => onChange(value)}
      style={{ display: 'inline-block', position: 'relative', top: 7, marginTop: -11 }}
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
