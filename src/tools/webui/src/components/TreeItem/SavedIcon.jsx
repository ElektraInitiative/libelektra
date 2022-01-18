/**
 * @file
 *
 * @brief small icon to notify the user that data was saved
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from "react";

import DoneIcon from "material-ui/svg-icons/action/done";
import ErrorIcon from "material-ui/svg-icons/navigation/close";

const SavedIcon = ({ saved, err, style }) => {
  const savedIconBaseStyle = {
    width: 16,
    height: 16,
    paddingTop: 1,
    paddingLeft: 4,
    color: "#00BCD4",
    transition: "opacity 0.5s",
  };

  const errorIconBaseStyle = {
    ...savedIconBaseStyle,
    color: "#FF4081",
  };

  const savedIconActiveStyle = saved ? { opacity: 1 } : { opacity: 0 };

  const savedIconStyle = {
    ...savedIconBaseStyle,
    ...savedIconActiveStyle,
    ...style,
  };
  const errorIconStyle = { ...errorIconBaseStyle, opacity: 1, ...style };

  if (err) {
    return <ErrorIcon className="savedIcon" style={errorIconStyle} />;
  }

  return <DoneIcon className="savedIcon" style={savedIconStyle} />;
};

export default SavedIcon;
