/**
 * @file
 *
 * @brief create a middleware that defines undo/redo actions
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { createUndoMiddleware } from 'redux-undo-redo-middleware'

import { setKey, deleteKey, createKey } from './actions'

const storePreviousValue = (state, { id, path }) => ({
  previousValue: state.kdb && state.kdb[id] && state.kdb[id][path] &&
                 state.kdb[id][path].value,
})

const undoMiddleware = createUndoMiddleware({
  revertingActions: {
    'SET_KEY_SUCCESS': {
      action: ({ id, path }, { previousValue }) => setKey(id, path, previousValue),
      createArgs: storePreviousValue,
    },
    'DELETE_KEY_SUCCESS': {
      action: ({ id, path }, { previousValue }) => createKey(id, path, previousValue),
      createArgs: storePreviousValue,
    },
    'CREATE_KEY_SUCCESS': ({ id, path }) => deleteKey(id, path),
    // TODO: moveKey
    // TODO: copyKey
    // TODO: setMetaKey
    // TODO: deleteMetaKey
    // TODO: createMetaKey
  },
})

export default undoMiddleware
