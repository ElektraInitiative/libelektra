/**
 * @file
 *
 * @brief interactive tree view item to edit configurations of instances
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'
import PropTypes from 'prop-types'

// TODO: render various input fields here
const TreeItem = ({ data, item, inputs }) =>
  (data && data.value)
    ? item.name + ': ' + data.value
    : item.name

export default TreeItem
