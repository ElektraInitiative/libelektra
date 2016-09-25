import React from 'react'
import ReactDOM from 'react-dom'

import injectTapEventPlugin from 'react-tap-event-plugin'
injectTapEventPlugin()

import { createStore, applyMiddleware } from 'redux'
import { createMiddleware } from 'redux-promises'
import { Provider } from 'react-redux'

import reducer from './reducers'
import { fetchInstances } from './actions'

const promisesMiddleware = createMiddleware()
const store = applyMiddleware(promisesMiddleware)(createStore)(reducer)

store.dispatch(fetchInstances())

import ConnectedApp from './containers/ConnectedApp'

ReactDOM.render(
  <Provider store={store}>
    <ConnectedApp />
  </Provider>,
  document.getElementById('main')
)
