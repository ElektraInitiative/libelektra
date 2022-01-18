/**
 * @file
 *
 * @brief handle actions related to operations on the Elektra key database (kdb)
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import {
  GET_KEY_SUCCESS,
  SET_KEY_SUCCESS,
  DELETE_KEY_SUCCESS,
  MOVE_KEY_SUCCESS,
  SET_META_SUCCESS,
  DELETE_META_SUCCESS,
  COPY_KEY_SUCCESS,
  CREATE_KEY_SUCCESS,
} from "../actions";

const updateState = (state, { id, path, value, meta, exists }) => {
  const updatedPart = {
    value:
      typeof value !== "undefined"
        ? value
        : state[id] && state[id][path] && state[id][path].value,
    meta:
      typeof meta !== "undefined"
        ? meta
        : state[id] && state[id][path] && state[id][path].meta,
    exists:
      typeof exists !== "undefined"
        ? exists
        : state[id] && state[id][path] && state[id][path].exists,
  };
  return {
    ...state,
    [id]: {
      ...state[id],
      [path]: updatedPart,
    },
  };
};

// controls the state of the local kdb cache
export default function keyReducer(state = {}, action) {
  switch (action.type) {
    case GET_KEY_SUCCESS:
      const newState = updateState(state, action.result);
      if (action.result && Array.isArray(action.result.children)) {
        return action.result.children.reduce(
          (res, c) => updateState(res, { ...c, id: action.result.id }),
          newState
        );
      }
      return newState;

    case CREATE_KEY_SUCCESS:
    case SET_KEY_SUCCESS:
      return updateState(state, action.request);

    case COPY_KEY_SUCCESS: {
      const { id, from, to } = action && action.request;
      const fromData = state[id] && state[id][from];
      return updateState(state, { ...fromData, id, path: to });
    }

    case SET_META_SUCCESS: {
      const { id, path, key, value } = action.request;
      const { meta } = state[id] && state[id][path];
      return updateState(state, { id, path, meta: { ...meta, [key]: value } });
    }

    case DELETE_META_SUCCESS: {
      const { id, path, key } = action.request;
      const { meta } = state[id] && state[id][path];
      return updateState(state, {
        id,
        path,
        meta: { ...meta, [key]: undefined },
      });
    }

    case DELETE_KEY_SUCCESS: {
      const { id, path } = action.request;
      return {
        ...state,
        [id]:
          state[id] &&
          Object.keys(state[id]).reduce((res, key) => {
            const data = state[id][key];
            const restricted =
              data &&
              data.meta &&
              data.meta["restrict/remove"] &&
              data.meta["restrict/remove"] === "1";
            if (key !== path || restricted) {
              res[key] = data;
            }
            return res;
          }, {}),
      };
    }

    case MOVE_KEY_SUCCESS: {
      const { id, from, to } = action && action.request;
      const fromData = state[id] && state[id][from];
      return {
        ...state,
        [id]:
          state[id] &&
          Object.keys(state[id]).reduce((res, key) => {
            if (key !== from) {
              // exclude `from` key
              if (key === to) {
                // move data to `to` key
                res[key] = fromData;
              } else {
                // otherwise, copy from state
                res[key] = state[id][key];
              }
            }
            return res;
          }, {}),
      };
    }

    // TODO: specifically handle failure (show error inline)
    default:
      return state;
  }
}
