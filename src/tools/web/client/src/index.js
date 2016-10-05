import React from 'react'
import ReactDOM from 'react-dom'

// enable onTouchTap events
//   this makes it possible to listen to events from mobile phone taps as well
//   as clicks on machines that don't have a touch screen
import injectTapEventPlugin from 'react-tap-event-plugin'
injectTapEventPlugin()

// initialize redux store
import configureStore from './store'
const store = configureStore()

// fetch instances when the app is loaded
import { fetchInstances, fetchClusters } from './actions'
store.dispatch(fetchInstances())
store.dispatch(fetchClusters())

// load and render the app
import { Provider } from 'react-redux'
import ConnectedApp from './containers/ConnectedApp'

ReactDOM.render(
  <Provider store={store}>
    <ConnectedApp />
  </Provider>,
  document.getElementById('main')
)
