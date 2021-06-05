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

import React from "react";

import MuiThemeProvider from "material-ui/styles/MuiThemeProvider";
import { BrowserRouter as Router, Route, withRouter } from "react-router-dom";

import { fetchInstance } from "../actions";
import Menu from "../containers/ConnectedMenu";
import ErrorSnackbar from "../containers/ConnectedErrorSnackbar";
import NotificationSnackbar from "../containers/ConnectedNotificationSnackbar";
import Home from "../containers/ConnectedHomePage";
import Configuration from "../containers/ConnectedConfigurationPage";

const getSubpage = ({ match }) => {
  const { path } = match && match.params;
  if (path.startsWith("instances")) {
    return "configuring instance";
  }
};

const getSingleInstance = ({ match }) => {
  const { id } = match && match.params;
  return id === "my";
};

class App extends React.Component {
  componentWillMount() {
    const { store, history } = this.props;
    // single instance mode
    store.dispatch(fetchInstance("my")).then((instance) => {
      if (!instance || instance.error) {
        console.log("single instance mode: %coff", "font-weight: bold");
      } else {
        history.push("/instances/my");
      }
    });
  }

  render() {
    return (
      <div>
        <Route exact path="/" component={Menu} />
        <Route
          exact
          path="/:path"
          render={(props) => <Menu subpage={getSubpage(props)} />}
        />
        <Route
          path="/:path/:id"
          render={(props) => (
            <Menu
              subpage={getSubpage(props)}
              singleInstanceMode={getSingleInstance(props)}
            />
          )}
        />
        <div style={{ padding: 50 }}>
          <Route exact path="/" component={Home} />
          <Route path="/instances/:id" component={Configuration} />
        </div>
        <NotificationSnackbar />
        <ErrorSnackbar />
      </div>
    );
  }
}

// this injects the `history` property into our App component
const RoutedApp = withRouter(App);

// wrap app with the Router and MuiThemeProvider (required for material-ui)
const WrappedApp = ({ store }) => (
  <Router>
    <MuiThemeProvider>
      <RoutedApp store={store} />
    </MuiThemeProvider>
  </Router>
);

export default WrappedApp;
