/**
 * @file
 *
 * @brief dialog which traps the focus (keyboard support)
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from "react";
import FocusTrap from "focus-trap-react";
import Dialog from "material-ui/Dialog";

const FocusTrapDialog = ({
  children,
  open,
  paused = false,
  onRequestClose,
  ...rest
}) => (
  <Dialog open={open} onRequestClose={onRequestClose} {...rest}>
    <FocusTrap
      active={open}
      paused={paused}
      focusTrapOptions={{
        clickOutsideDeactivates: true,
        escapeDeactivates: false,
        returnFocusOnDeactivate: false,
      }}
    >
      {children}
    </FocusTrap>
  </Dialog>
);

export default FocusTrapDialog;
