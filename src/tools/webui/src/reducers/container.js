/**
 * @file
 *
 * @brief handle actions in the main overview (container of instances)
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import {
  ADD_INSTANCE,
  UNADD_INSTANCE,
  CREATE_INSTANCE_SUCCESS,
} from "../actions";

// controls the state of the card container (for adding instances)
export default function containerReducer(
  state = { addingInstance: false },
  action
) {
  switch (action.type) {
    case ADD_INSTANCE:
      return { ...state, addingInstance: true };

    case UNADD_INSTANCE:
    case CREATE_INSTANCE_SUCCESS:
      return { ...state, addingInstance: false };

    default:
      return state;
  }
}
