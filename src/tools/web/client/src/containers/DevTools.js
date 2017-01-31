/**
 * @file
 *
 * @brief create redux devtools (sidebar in development mode)
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

import React from 'react'

import { createDevTools } from 'redux-devtools'
import Inspector from 'redux-devtools-inspector'

export default createDevTools(
  React.createElement(Inspector)
)
