/* containers/DevTools.js
create redux devtools (sidebar in development mode)
*/

import React from 'react'

import { createDevTools } from 'redux-devtools'
import Inspector from 'redux-devtools-inspector'

export default createDevTools(
  React.createElement(Inspector)
)
