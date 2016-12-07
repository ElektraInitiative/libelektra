import React from 'react'

import { createDevTools } from 'redux-devtools'
import Inspector from 'redux-devtools-inspector'

export default createDevTools(
  React.createElement(Inspector)
)
