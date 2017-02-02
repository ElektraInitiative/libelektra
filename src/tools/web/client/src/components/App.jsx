/**
 * @file
 *
 * @brief this is the main component of the application
 *
 * it renders the overview page or the configuration page, depending on the
 * router state. it also renders the DevTools sidebar in development mode.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

import React from 'react'

import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider'

import ConnectedMenu from '../containers/ConnectedMenu'
import ConnectedContainer from '../containers/ConnectedContainer'
import ConnectedConfiguration from '../containers/ConnectedConfiguration'
import ConnectedErrorSnackbar from '../containers/ConnectedErrorSnackbar'

import DevTools from '../containers/DevTools'
import Paper from 'material-ui/Paper'

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
const getSubpageName = ({ page, configuring, id }) => {
  switch (page) {
    case PAGE_CONFIGURE:
      return `${configuring} #${id}`
    default:
    case PAGE_MAIN:
      return false
  }
}

// this is the main application
const MainApp = (props) =>
    <div>
        <ConnectedMenu subpage={getSubpageName(props)} />
        <div style={{padding: '50px'}}>
            {displayPage(props)}
        </div>
        <ConnectedErrorSnackbar />
    </div>

// this is the main application wrapped inside devtools (for development mode)
const DevApp = (props) =>
    <div>
        <div style={{display: 'inline-block', width: '80%'}}>
            <MainApp {...props} />
        </div>
        <Paper style={{
          display: 'inline-block',
          width: '20%',
          height: '100vh',
          position: 'absolute',
          top: 0,
        }}>
            <DevTools />
        </Paper>
    </div>

// show only MainApp in production mode,
// and DevApp (MainApp with devtools) in development mode
const AppContainer = (props) =>
  process.env.NODE_ENV === 'production'
    ? <MainApp {...props} />
    : <DevApp {...props} />

// wrap app container with the MuiThemeProvider (required for material-ui)
const App = (props) =>
    <MuiThemeProvider>
        <AppContainer {...props} />
    </MuiThemeProvider>

export default App
