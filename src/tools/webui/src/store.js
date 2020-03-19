/**
 * @file
 *
 * @brief initialize the redux store
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { createStore, applyMiddleware } from "redux";
import { createMiddleware as createPromisesMiddleware } from "redux-promises";
import logger from "redux-logger";

import reducer from "./reducers";
import undoMiddleware from "./undo";

// create middleware store enhancer
const promiseMiddleware = createPromisesMiddleware(); // allow returning promises from action creators
const middleware =
  process.env.NODE_ENV === "production"
    ? applyMiddleware(promiseMiddleware, undoMiddleware)
    : applyMiddleware(promiseMiddleware, undoMiddleware, logger);

// configure redux store
export default function configureStore(initialState) {
  const store = createStore(reducer, initialState, middleware);

  // hot-reload reducers in development mode
  if (process.env.NODE_ENV !== "production" && module.hot) {
    module.hot.accept("./reducers", () =>
      store.replaceReducer(require("./reducers").default)
    );
  }

  return store;
}
