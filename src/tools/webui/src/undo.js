/**
 * @file
 *
 * @brief create a middleware that defines undo/redo actions
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { createUndoMiddleware } from "redux-undo-redo-middleware";

import {
  setKey,
  deleteKey,
  createKey,
  copyKey,
  moveKey,
  setMetaKey,
  deleteMetaKey,
  createMetaKey,
} from "./actions";

const storePreviousValue = (state, { id, path }) => ({
  previousValue:
    state.kdb &&
    state.kdb[id] &&
    state.kdb[id][path] &&
    state.kdb[id][path].value,
});

const storePreviousMeta = (state, { id, path, key }) => ({
  previousMeta:
    state.kdb &&
    state.kdb[id] &&
    state.kdb[id][path] &&
    state.kdb[id][path].meta &&
    state.kdb[id][path].meta[key],
});

const undoMiddleware = createUndoMiddleware({
  revertingActions: {
    SET_KEY_SUCCESS: {
      action: ({ id, path }, { previousValue }) =>
        setKey(id, path, previousValue),
      createArgs: storePreviousValue,
    },
    RESET_KEY_SUCCESS: {
      // when re-doing set actions, set back to original value
      action: ({ id, path, value }) => setKey(id, path, value),
    },
    DELETE_KEY_SUCCESS: {
      action: ({ id, path, kdb }, { previousValue, from, to }) =>
        from && to // we are reverting a copy action
          ? copyKey(id, from, to)
          : createKey(id, path, previousValue, kdb),
      createArgs: storePreviousValue,
    },
    CREATE_KEY_SUCCESS: {
      action: ({ id, path, kdb }) => deleteKey(id, path, kdb),
      createArgs: (state, { value }) => ({ previousValue: value }),
    },
    SET_META_SUCCESS: {
      action: ({ id, path, key }, { previousMeta }) =>
        previousMeta
          ? setMetaKey(id, path, key, previousMeta)
          : deleteMetaKey(id, path, key), // there was no previous value -> delete
      createArgs: storePreviousMeta,
    },
    RESET_META_SUCCESS: {
      // when re-doing meta set actions, set back to original value
      action: ({ id, path, key, value }) => setMetaKey(id, path, key, value),
    },
    DELETE_META_SUCCESS: {
      action: ({ id, path, key }, { previousMeta }) =>
        createMetaKey(id, path, key, previousMeta),
      createArgs: storePreviousMeta,
    },
    CREATE_META_SUCCESS: {
      action: ({ id, path, key }) => deleteMetaKey(id, path, key),
      createArgs: (state, { value }) => ({ previousMeta: value }),
    },
    COPY_KEY_SUCCESS: {
      action: ({ id, from, to }) => deleteKey(id, to),
      createArgs: (state, { from, to }) => ({ from, to }),
    },
    MOVE_KEY_SUCCESS: {
      action: ({ id, from, to }) => moveKey(id, to, from),
    },
    RESET_MOVE_KEY_SUCCESS: {
      action: ({ id, from, to }) => moveKey(id, from, to),
    },
  },
  originalActions: {
    SET_KEY_SUCCESS: "RESET_KEY_SUCCESS",
    DELETE_KEY_SUCCESS: "CREATE_KEY_SUCCESS",
    CREATE_KEY_SUCCESS: "DELETE_KEY_SUCCESS",
    SET_META_SUCCESS: "RESET_META_SUCCESS",
    DELETE_META_SUCCESS: "CREATE_META_SUCCESS",
    CREATE_META_SUCCESS: "DELETE_META_SUCCESS",
    COPY_KEY_SUCCESS: "DELETE_KEY_SUCCESS",
    MOVE_KEY_SUCCESS: "RESET_MOVE_KEY_SUCCESS",
  },
});

export default undoMiddleware;
