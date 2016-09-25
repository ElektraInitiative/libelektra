import React from 'react'

import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider'

import ConnectedMenu from '../containers/ConnectedMenu'
import ConnectedContainer from '../containers/ConnectedContainer'
import ConnectedConfiguration from '../containers/ConnectedConfiguration'

import { PAGE_MAIN, PAGE_CONFIGURE } from '../router'

const displayPage = ({ page, ...instance }) => {
  switch (page) {
    case PAGE_CONFIGURE:
      return <ConnectedConfiguration {...instance} />
    default:
    case PAGE_MAIN:
      return <ConnectedContainer />
  }
}

const getSubpageName = ({ page, id }) => {
  switch (page) {
    case PAGE_CONFIGURE:
      return `instance #${id}`
    default:
    case PAGE_MAIN:
      return false
  }
}

const App = (props) =>
    <MuiThemeProvider>
        <div>
            <ConnectedMenu subpage={getSubpageName(props)} />
            <div style={{padding: '50px'}}>
                {displayPage(props)}
            </div>
        </div>
    </MuiThemeProvider>

export default App
