/**
 * @file
 *
 * @brief dialog to modify the value of keys with sub-keys
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from "react";

import FlatButton from "material-ui/FlatButton";
import TextField from "material-ui/TextField";
import FocusTrapDialog from "./FocusTrapDialog.jsx";

export default class RenameDialog extends Component {
  constructor(props, ...args) {
    super(props, ...args);
    this.state = {
      name: (props.item && props.item.name) || "",
    };
  }

  handleRename = () => {
    const { onRename, onClose } = this.props;
    const { name } = this.state;
    onRename(name);
    onClose();
  };

  render() {
    const { item, open, onClose } = this.props;
    const { path } = item;
    const { name } = this.state;
    const nameEmpty = !name || name.trim().length <= 0;

    const actions = [
      <FlatButton
        label="Cancel"
        onClick={onClose}
        onKeyPress={(e) => {
          if (e.key === "Enter") {
            onClose();
          }
        }}
      />,
      <FlatButton
        label="Done"
        primary={true}
        onClick={this.handleRename}
        disabled={nameEmpty}
        onKeyPress={(e) => {
          if (e.key === "Enter") {
            this.handleRename();
          }
        }}
      />,
    ];

    return (
      <FocusTrapDialog
        actions={actions}
        modal={false}
        open={open}
        paused={true}
        onRequestClose={onClose}
      >
        <h1>
          Rename <b>{path}</b>
        </h1>
        <div style={{ display: "block" }}>
          <TextField
            ref="nameField"
            floatingLabelText="new name"
            floatingLabelFixed={true}
            hintText="e.g. keyName"
            onChange={(evt) => this.setState({ name: evt.target.value })}
            value={name}
            onKeyPress={(e) => {
              if (!nameEmpty && e.key === "Enter") {
                this.handleRename();
              }
            }}
          />
        </div>
      </FocusTrapDialog>
    );
  }
}
