import React from 'react'

import MuiThemeProvider from 'material-ui/styles/MuiThemeProvider'

import ConnectedMenu from '../containers/ConnectedMenu'
import ConnectedContainer from '../containers/ConnectedContainer'
import ConnectedConfiguration from '../containers/ConnectedConfiguration'

import DevTools from '../containers/DevTools'
import Paper from 'material-ui/Paper'

import { PAGE_MAIN, PAGE_CONFIGURE } from '../router'

const displayPage = ({ page, ...instance }) => {
  console.log(page)
  switch (page) {
    case PAGE_CONFIGURE:
      return <ConnectedConfiguration {...instance} />
    default:
    case PAGE_MAIN:
      return <ConnectedContainer />
  }
}

const getSubpageName = ({ page, configuring, id }) => {
  switch (page) {
    case PAGE_CONFIGURE:
      return `${configuring} #${id}`
    default:
    case PAGE_MAIN:
      return false
  }
}

const MainApp = (props) =>
    <div>
        <ConnectedMenu subpage={getSubpageName(props)} />
        <div style={{padding: '50px'}}>
            {displayPage(props)}
        </div>
    </div>

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

const AppContainer = (props) =>
  process.env.NODE_ENV === 'production'
  ? <MainApp {...props} />
  : <DevApp {...props} />

const App = (props) =>
    <MuiThemeProvider>
        <AppContainer {...props} />
    </MuiThemeProvider>

export default App
