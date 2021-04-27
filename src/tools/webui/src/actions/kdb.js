/**
 * @file
 *
 * @brief Elektra key database (kdb) specific actions, used in the tree view
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { thunkCreator, encodePath, parseJSONResponse } from "./utils";

// ~~~

export const GET_KDB_REQUEST = "GET_KDB_REQUEST";
export const GET_KDB_SUCCESS = "GET_KDB_SUCCESS";
export const GET_KDB_FAILURE = "GET_KDB_FAILURE";

export const getKdb = (id) =>
  thunkCreator({
    id,
    request: { id },
    types: [GET_KDB_REQUEST, GET_KDB_SUCCESS, GET_KDB_FAILURE],
    promise: fetch(`/api/instances/${id}/kdb`, { credentials: "same-origin" })
      .then(parseJSONResponse)
      .then((result) => {
        return { ...result, id };
      }),
  });

// ~~~

export const GET_KEY_REQUEST = "GET_KEY_REQUEST";
export const GET_KEY_SUCCESS = "GET_KEY_SUCCESS";
export const GET_KEY_FAILURE = "GET_KEY_FAILURE";

export const getKey = (id, path, preload = false) =>
  thunkCreator({
    id,
    path,
    preload,
    request: { id, path, preload },
    types: [GET_KEY_REQUEST, GET_KEY_SUCCESS, GET_KEY_FAILURE],
    promise: fetch(
      `/api/instances/${id}/kdb/${encodePath(path)}${
        preload ? "?preload=1" : ""
      }`,
      { credentials: "same-origin" }
    )
      .then(parseJSONResponse)
      .then((result) => {
        return { ...result, id, path };
      }),
  });

// ~~~

export const SET_KEY_REQUEST = "SET_KEY_REQUEST";
export const SET_KEY_SUCCESS = "SET_KEY_SUCCESS";
export const SET_KEY_FAILURE = "SET_KEY_FAILURE";

export const setKey = (id, path, value) =>
  thunkCreator({
    id,
    path,
    value,
    request: { id, path, value },
    types: [SET_KEY_REQUEST, SET_KEY_SUCCESS, SET_KEY_FAILURE],
    promise: fetch(`/api/instances/${id}/kdb/${encodePath(path)}`, {
      credentials: "same-origin",
      method: "PUT",
      headers: {
        "Content-Type": "text/plain",
      },
      body: value,
    }).then(parseJSONResponse),
  });

// ~~~

export const CREATE_KEY_REQUEST = "CREATE_KEY_REQUEST";
export const CREATE_KEY_SUCCESS = "CREATE_KEY_SUCCESS";
export const CREATE_KEY_FAILURE = "CREATE_KEY_FAILURE";

export const createKey = (id, path, value, kdb) =>
  thunkCreator({
    id,
    path,
    value,
    kdb,
    request: { id, path, value },
    types: [CREATE_KEY_REQUEST, CREATE_KEY_SUCCESS, CREATE_KEY_FAILURE],
    promise: fetch(`/api/instances/${id}/kdb/${encodePath(path)}`, {
      credentials: "same-origin",
      method: "PUT",
      headers: {
        "Content-Type": "text/plain",
      },
      body: value,
    }).then(parseJSONResponse),
  });

// ~~~

export const DELETE_KEY_REQUEST = "DELETE_KEY_REQUEST";
export const DELETE_KEY_SUCCESS = "DELETE_KEY_SUCCESS";
export const DELETE_KEY_FAILURE = "DELETE_KEY_FAILURE";

export const deleteKey = (id, path, kdb) =>
  thunkCreator({
    id,
    path,
    kdb,
    request: { id, path },
    types: [DELETE_KEY_REQUEST, DELETE_KEY_SUCCESS, DELETE_KEY_FAILURE],
    promise: fetch(`/api/instances/${id}/kdb/${encodePath(path)}`, {
      credentials: "same-origin",
      method: "DELETE",
    }).then(parseJSONResponse),
  });

// ~~~

export const FIND_KEY_REQUEST = "FIND_KEY_REQUEST";
export const FIND_KEY_SUCCESS = "FIND_KEY_SUCCESS";
export const FIND_KEY_FAILURE = "FIND_KEY_FAILURE";

export const findKey = (id, query) =>
  thunkCreator({
    id,
    query,
    request: { id, query },
    types: [FIND_KEY_REQUEST, FIND_KEY_SUCCESS, FIND_KEY_FAILURE],
    promise: fetch(
      `/api/instances/${id}/kdbFind/${encodeURIComponent(query)}`,
      { credentials: "same-origin" }
    )
      .then(parseJSONResponse)
      .then((result) => {
        return { result, id, query };
      }),
  });

export const CLEAR_SEARCH = "CLEAR_SEARCH";
export const CLEAR_SEARCH_FINAL = "CLEAR_SEARCH_FINAL";

export const clearSearch = () => (dispatch) => {
  dispatch({ type: CLEAR_SEARCH });
  setTimeout(() => dispatch({ type: CLEAR_SEARCH_FINAL }), 200);
};

// ~~~

export const MOVE_KEY_REQUEST = "MOVE_KEY_REQUEST";
export const MOVE_KEY_SUCCESS = "MOVE_KEY_SUCCESS";
export const MOVE_KEY_FAILURE = "MOVE_KEY_FAILURE";

export const moveKey = (id, from, to) =>
  thunkCreator({
    id,
    from,
    to,
    request: { id, from, to },
    types: [MOVE_KEY_REQUEST, MOVE_KEY_SUCCESS, MOVE_KEY_FAILURE],
    promise: fetch(`/api/instances/${id}/kdbMv/${encodePath(from)}`, {
      credentials: "same-origin",
      method: "POST",
      headers: {
        "Content-Type": "text/plain",
      },
      body: to,
    }),
  });

// ~~~

export const COPY_KEY_REQUEST = "COPY_KEY_REQUEST";
export const COPY_KEY_SUCCESS = "COPY_KEY_SUCCESS";
export const COPY_KEY_FAILURE = "COPY_KEY_FAILURE";

export const copyKey = (id, from, to) =>
  thunkCreator({
    id,
    from,
    to,
    request: { id, from, to },
    types: [COPY_KEY_REQUEST, COPY_KEY_SUCCESS, COPY_KEY_FAILURE],
    promise: fetch(`/api/instances/${id}/kdbCp/${encodePath(from)}`, {
      credentials: "same-origin",
      method: "POST",
      headers: {
        "Content-Type": "text/plain",
      },
      body: to,
    }),
  });

// ~~~

export const CREATE_META_REQUEST = "CREATE_META_REQUEST";
export const CREATE_META_SUCCESS = "CREATE_META_SUCCESS";
export const CREATE_META_FAILURE = "CREATE_META_FAILURE";

export const createMetaKey = (id, path, key, value) =>
  thunkCreator({
    id,
    path,
    key,
    value,
    request: { id, path, key, value },
    types: [CREATE_META_REQUEST, CREATE_META_SUCCESS, CREATE_META_FAILURE],
    promise: fetch(`/api/instances/${id}/kdbMeta/${encodePath(path)}`, {
      credentials: "same-origin",
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({ key, value }),
    }),
  });

// ~~~

export const SET_META_REQUEST = "SET_META_REQUEST";
export const SET_META_SUCCESS = "SET_META_SUCCESS";
export const SET_META_FAILURE = "SET_META_FAILURE";

export const setMetaKey = (id, path, key, value) =>
  thunkCreator({
    id,
    path,
    key,
    value,
    request: { id, path, key, value },
    types: [SET_META_REQUEST, SET_META_SUCCESS, SET_META_FAILURE],
    promise: fetch(`/api/instances/${id}/kdbMeta/${encodePath(path)}`, {
      credentials: "same-origin",
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({ key, value }),
    }),
  });

// ~~~

export const DELETE_META_REQUEST = "DELETE_META_REQUEST";
export const DELETE_META_SUCCESS = "DELETE_META_SUCCESS";
export const DELETE_META_FAILURE = "DELETE_META_FAILURE";

export const deleteMetaKey = (id, path, key) =>
  thunkCreator({
    id,
    path,
    key,
    request: { id, path, key },
    types: [DELETE_META_REQUEST, DELETE_META_SUCCESS, DELETE_META_FAILURE],
    promise: fetch(`/api/instances/${id}/kdbMeta/${encodePath(path)}`, {
      credentials: "same-origin",
      method: "DELETE",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({ key }),
    }),
  });
