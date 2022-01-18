/**
 * @file
 *
 * @brief dialog to modify the value of keys with sub-keys
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from "react";

import FlatButton from "material-ui/FlatButton";
import FocusTrapDialog from "./FocusTrapDialog.jsx";
import { fromElektraBool } from "../../../utils";

export default class EditDialog extends Component {
  handleAbort = () => {
    const { batchUndo, sendNotification, onUndo, onClose, refreshKey } =
      this.props;
    onClose();
    const steps = [];
    for (let i = 0; i < batchUndo; i++) {
      steps.push(i);
    }
    Promise.all(steps.map(onUndo)).then(() => {
      sendNotification("Reverted " + batchUndo + " changes.");
      refreshKey();
    });
  };

  render() {
    const { item, open, renderField, onClose, meta } = this.props;
    const { path } = item;

    const actions = [
      <FlatButton
        label="Abort"
        secondary={true}
        onClick={this.handleAbort}
        onKeyPress={(e) => {
          if (e.key === "Enter") {
            this.handleAbort();
          }
        }}
      />,
      <FlatButton
        label="Done"
        primary={true}
        onClick={onClose}
        onKeyPress={(e) => {
          if (e.key === "Enter") {
            onClose();
          }
        }}
      />,
    ];

    const isDisabled =
      (meta && meta.hasOwnProperty("binary")) ||
      fromElektraBool(meta && meta["restrict/write"]);

    return (
      <FocusTrapDialog
        actions={actions}
        modal={false}
        open={open}
        paused={true}
        onRequestClose={onClose}
      >
        <h1>
          Value of <b>{path}</b>
        </h1>
        <div style={{ display: "block" }} tabIndex={isDisabled && "0"}>
          {renderField({
            onKeyPress: (e) => {
              if (e.key === "Enter") {
                onClose();
              }
            },
          })}
        </div>
      </FocusTrapDialog>
    );
  }
}
