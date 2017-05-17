/**
 * @file
 *
 * @brief create redux devtools (sidebar in development mode)
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'

import { createDevTools } from 'redux-devtools'
import Inspector from 'redux-devtools-inspector'

export default createDevTools(
  React.createElement(Inspector)
)
