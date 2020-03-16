/**
 * @file
 *
 * @brief small icon to signify array keys to the user
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from "react";
import ActionTOC from "material-ui/svg-icons/action/toc";
import IconButton from "material-ui/IconButton";

const ArrayIcon = () => (
  <IconButton
    style={{ width: 24, height: 24, padding: 3, cursor: "auto" }}
    iconStyle={{ width: 18, height: 18, opacity: 0.3 }}
    tooltip="this is an array"
  >
    <ActionTOC />
  </IconButton>
);

export default ArrayIcon;
