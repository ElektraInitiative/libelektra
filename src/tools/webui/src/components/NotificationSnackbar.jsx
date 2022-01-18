/**
 * @file
 *
 * @brief shows notifications in a small snackbar on the bottom of the UI
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from "react";
import Snackbar from "material-ui/Snackbar";

const fromTop = (message) => ({
  top: 0,
  bottom: "auto",
  left: (window.innerWidth - 288) / 2,
  transform: message ? "translate3d(0, 0, 0)" : `translate3d(0, -50px, 0)`,
});

const NotificationSnackbar = ({ message }) => {
  return (
    <Snackbar
      open={!!message}
      message={message}
      autoHideDuration={4500}
      style={fromTop(message)}
    />
  );
};

export default NotificationSnackbar;
