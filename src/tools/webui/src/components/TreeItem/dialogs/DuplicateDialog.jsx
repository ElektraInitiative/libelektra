/**
 * @file
 *
 * @brief dialog to duplicate a key
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from "react";

import FlatButton from "material-ui/FlatButton";
import TextField from "material-ui/TextField";
import FocusTrapDialog from "./FocusTrapDialog.jsx";

export default class DuplicateDialog extends Component {
  constructor(...args) {
    super(...args);
    const [props] = args;
    this.state = {
      name: this.getInitialName(props),
    };
  }

  generateArrayKey = (length) => {
    const numberStr = String(length);
    const prefix = "_".repeat(numberStr.length - 1);
    return "#" + prefix + length;
  };

  getInitialName = (props) => {
    if (props.arrayKeyLength) {
      return this.generateArrayKey(props.arrayKeyLength);
    }
    return props && props.item && props.item.name
      ? props.item.name + "Copy"
      : "";
  };

  handleClose = () => {
    const { onClose } = this.props;
    this.setState({ name: this.getInitialName(this.props) });
    onClose();
  };

  handleDuplicate = (path, newPath) => {
    const { onDuplicate, pathExists } = this.props;

    if (path === newPath) {
      return alert("Cannot duplicate to the same key name!");
    }
    if (pathExists(newPath)) {
      return alert("Cannot duplicate to a path that already exists!");
    }

    onDuplicate(path, newPath);
    this.handleClose();
  };

  render() {
    const { item, open } = this.props;
    const { path } = item;
    const { name } = this.state;

    const newPathParts = path.split("/");
    newPathParts.pop();
    const newPath = newPathParts.join("/") + "/" + name;

    const actions = [
      <FlatButton
        label="Cancel"
        onClick={this.handleClose}
        onKeyPress={(e) => {
          if (e.key === "Enter") {
            this.handleClose();
          }
        }}
      />,
      <FlatButton
        label="Duplicate"
        primary={true}
        onClick={() => this.handleDuplicate(path, newPath)}
        onKeyPress={(e) => {
          if (e.key === "Enter") {
            this.handleDuplicate(path, newPath);
          }
        }}
      />,
    ];

    return (
      <FocusTrapDialog
        actions={actions}
        modal={false}
        open={open || false}
        paused={true}
        onRequestClose={this.handleClose}
      >
        <h1>
          Duplicating <b>{path}</b> key
        </h1>
        <div style={{ display: "block" }}>
          <TextField
            ref="nameField"
            floatingLabelText="name of copy"
            floatingLabelFixed={true}
            hintText="e.g. keyNameCopy"
            onChange={(evt) => this.setState({ name: evt.target.value })}
            onKeyPress={(e) => {
              if (e.key === "Enter") {
                this.handleDuplicate(path, newPath);
              }
            }}
            value={name}
          />
        </div>
      </FocusTrapDialog>
    );
  }
}
