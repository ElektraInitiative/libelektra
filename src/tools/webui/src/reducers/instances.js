/**
 * @file
 *
 * @brief handle actions related to instances
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import {
  INSTANCES_SUCCESS,
  INSTANCE_DELETE_SUCCESS,
  INSTANCE_UPDATE_SUCCESS,
  CREATE_INSTANCE_SUCCESS,
} from "../actions";

export default function instancesReducer(state = [], action) {
  switch (action.type) {
    case INSTANCE_DELETE_SUCCESS: // instance deleted, remove from state
      return state.filter((instance) => instance.id !== action.result.id);

    case INSTANCE_UPDATE_SUCCESS: // instance updated, update in state
      return state.map((instance) => {
        if (instance.id === action.result.id) {
          const hasDifference = // ignore differences in unfolded
            instance.name !== action.result.name ||
            instance.description !== action.result.description ||
            instance.host !== action.result.host ||
            instance.visibility !== action.result.visibility;
          if (hasDifference) {
            return action.result;
          }
        }
        return instance;
      });

    case INSTANCES_SUCCESS: // instance list pulled from webd, update state
      return action.result;

    case CREATE_INSTANCE_SUCCESS: // instance created, add to state
      return [...state, action.result];

    default:
      return state;
  }
}
