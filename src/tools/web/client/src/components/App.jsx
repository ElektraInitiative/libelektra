/**
 * @file
 *
 * @brief this is the main component of the application
 *
 * it renders the overview page or the configuration page, depending on the
 * router state.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'

import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider'

import ConnectedMenu from '../containers/ConnectedMenu'
import ConnectedContainer from '../containers/ConnectedContainer'
import ConnectedConfiguration from '../containers/ConnectedConfiguration'
import ConnectedErrorSnackbar from '../containers/ConnectedErrorSnackbar'

import { PAGE_MAIN, PAGE_CONFIGURE } from '../router'

// mini router that displays the confiugration/main page
const displayPage = ({ page, ...instance }) => {
  switch (page) {
    case PAGE_CONFIGURE:
      return <ConnectedConfiguration {...instance} />
    default:
    case PAGE_MAIN:
      return <ConnectedContainer />
  }
}

// get name of the current page for the breadcrumb
const getSubpageName = ({ page, id }) => {
  switch (page) {
    case PAGE_CONFIGURE:
      return `instance #${id}`

    default:
    case PAGE_MAIN:
      return false
  }
}

// wrap app with the MuiThemeProvider (required for material-ui)
const App = (props) =>
    <MuiThemeProvider>
        <div>
            <ConnectedMenu subpage={getSubpageName(props)} />
            <div style={{padding: '50px'}}>
                {displayPage(props)}
            </div>
            <ConnectedErrorSnackbar />
        </div>
    </MuiThemeProvider>

export default App
