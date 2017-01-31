/* store.js
initialize the redux store
*/

import { createStore, applyMiddleware, compose } from 'redux'
import { createMiddleware as createPromisesMiddleware } from 'redux-promises'

import DevTools from './containers/DevTools'
import reducer from './reducers'

// create middleware store enhancer
const middleware = applyMiddleware(
  createPromisesMiddleware() // allow returning promises from action creators
)

// compose store enhancers
const enhancer = process.env.NODE_ENV === 'production'
  ? middleware
  : compose(middleware, DevTools.instrument()) // enable devtools store enhancer

// configure redux store
export default function configureStore (initialState) {
  const store = createStore(reducer, initialState, enhancer)

  // hot-reload reducers in development mode
  if (process.env.NODE_ENV !== 'production' && module.hot) {
    module.hot.accept('./reducers', () =>
      store.replaceReducer(require('./reducers').default)
    )
  }

  return store
}
