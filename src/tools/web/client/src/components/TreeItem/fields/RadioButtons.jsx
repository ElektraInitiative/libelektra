/**
 * @file
 *
 * @brief radio buttons for tree items
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'

import { RadioButton, RadioButtonGroup } from 'material-ui/RadioButton'

import { fromElektraBool } from '../../../utils'

const RadioButtons = ({ id, value, meta, options, onChange }) => (
    <RadioButtonGroup
      id={id}
      name={id}
      defaultSelected={value}
      onChange={(evt, value) => onChange(value)}
      style={{ display: 'inline-block', position: 'relative', top: 7, marginTop: -11 }}
    >
        {options.map(option =>
            <RadioButton
              key={id + '-' + option}
              value={option}
              label={option}
              style={{ display: 'inline-block', width: 'auto', paddingRight: 32 }}
              disabled={fromElektraBool(meta && meta.readonly)}
            />
        )}
    </RadioButtonGroup>
)

export default RadioButtons
