/**
 * @file
 *
 * @brief handle notifications
 *
 * they will be shown in a small snackbar at the bottom of the UI
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { SEND_NOTIFICATION } from "../actions";

export default function notificationReducer(state = false, action) {
  switch (action.type) {
    case SEND_NOTIFICATION:
      return action.message;

    default:
      return state;
  }
}
