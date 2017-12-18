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
import { BrowserRouter as Router, Route } from 'react-router-dom'

import Menu from '../containers/ConnectedMenu'
import ErrorSnackbar from '../containers/ConnectedErrorSnackbar'
import Home from '../containers/ConnectedHomePage'
import Configuration from '../containers/ConnectedConfigurationPage'

const getSubpage = ({ match }) => {
  const { path } = match && match.params
  if (path.startsWith('instances')) {
    return 'configuring instance'
  }
}

// wrap app with the MuiThemeProvider (required for material-ui)
const App = () =>
  <Router>
    <MuiThemeProvider>
        <div>
            <Route exact path="/" component={Menu} />
            <Route path="/:path" render={props =>
                <Menu subpage={getSubpage(props)} />
            } />
            <div style={{ padding: 50 }}>
                <Route exact path="/" component={Home} />
                <Route path="/instances/:id" component={Configuration} />
            </div>
            <ErrorSnackbar />
        </div>
    </MuiThemeProvider>
  </Router>

export default App
