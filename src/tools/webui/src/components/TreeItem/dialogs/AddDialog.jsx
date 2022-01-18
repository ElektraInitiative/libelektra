/**
 * @file
 *
 * @brief dialog to create a new key
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from "react";

import FlatButton from "material-ui/FlatButton";
import TextField from "material-ui/TextField";
import SelectField from "material-ui/SelectField";
import MenuItem from "material-ui/MenuItem";
import ActionBuild from "material-ui/svg-icons/action/build";
import FocusTrapDialog from "./FocusTrapDialog.jsx";

import { VISIBILITY_LEVELS, visibility } from "../../../utils";
import { KEY_TYPES } from "./utils";

export default class AddDialog extends Component {
  constructor(props, ...args) {
    super(props, ...args);
    this.state = {
      name: "",
      value: "",
      type: "any",
      visibility: props.instanceVisibility || "user",
      error: false,
      paused: false,
    };
  }

  generateArrayKey = (length) => {
    const numberStr = String(length);
    const prefix = "_".repeat(numberStr.length - 1);
    return "#" + prefix + length;
  };

  componentWillReceiveProps(nextProps) {
    const wasOpened = this.props.open === false && nextProps.open === true;
    if (wasOpened && nextProps.arrayKeyLength !== false) {
      this.setState({ name: this.generateArrayKey(nextProps.arrayKeyLength) });
    }
  }

  handleClose = () => {
    const { onClose, instanceVisibility } = this.props;
    this.setState({
      name: "",
      value: "",
      type: "any",
      visibility: instanceVisibility || "user",
      error: false,
    });
    onClose();
  };

  handleCreate = (arrayKey = false) => {
    const { item, onAdd, keyExists, setMetaByPath, arrayKeyLength } =
      this.props;
    const { path } = item;
    const { name, value, type } = this.state;
    const v = this.state.visibility;
    if (v !== "user") {
      const { instanceVisibility } = this.props;
      if (visibility(v) < visibility(instanceVisibility)) {
        const confirmed = window.confirm(
          "Setting the visibility lower than the instance visibility will hide " +
            "this item in this instance. Only proceed if you no longer plan on " +
            "editing this item here."
        );
        if (!confirmed) return;
      }
    }
    keyExists(path, name).then((res) => {
      if (res && res.exists) {
        alert(
          'A key with the name "' +
            name +
            '" at path "' +
            path +
            '" already exists! Please choose a different name.'
        );
        return;
      }
      onAdd(path, name, value);
      if (type !== "any") {
        setMetaByPath(path + "/" + name, "check/type", type);
      }
      if (v !== "user") {
        setMetaByPath(path + "/" + name, "visibility", v);
      }
      if (arrayKeyLength !== false) {
        // is child of array key
        // update array metakey in parent
        setMetaByPath(path, "array", String(arrayKeyLength + 1));
      }
      if (arrayKey === true) {
        setMetaByPath(path + "/" + name, "array", "1");
        setTimeout(() => onAdd(path + "/" + name, "#0", ""), 250);
      }
      this.handleClose();
    });
  };

  handleCreateArrayKey = () => {
    this.handleCreate(true);
  };

  render() {
    const { item, open, renderField, arrayKeyLength } = this.props;
    const { path } = item;
    const { name, value, type, error, visibility } = this.state;

    const nameEmpty = !name || name.trim().length <= 0;

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
        label="Create array"
        primary={true}
        onClick={this.handleCreateArrayKey}
        onKeyPress={(e) => {
          if (e.key === "Enter") {
            this.handleCreateArrayKey();
          }
        }}
        disabled={nameEmpty || error}
      />,
      <FlatButton
        label="Create"
        primary={true}
        onClick={this.handleCreate}
        onKeyPress={(e) => {
          if (e.key === "Enter") {
            this.handleCreate();
          }
        }}
        disabled={nameEmpty || error}
      />,
    ];

    return (
      <FocusTrapDialog
        actions={actions}
        modal={false}
        open={open}
        paused={this.state.paused}
        onRequestClose={this.handleClose}
      >
        <h1>
          Creating new {arrayKeyLength ? "array " : ""}key at <b>{path}</b>
        </h1>
        <div style={{ display: "flex" }}>
          <div style={{ flex: 1 }}>
            <TextField
              ref="nameField"
              floatingLabelText="name"
              floatingLabelFixed={true}
              hintText="e.g. keyName"
              onChange={(evt) => this.setState({ name: evt.target.value })}
              value={name}
              onKeyPress={(e) => {
                if (!nameEmpty && !error && e.key === "Enter") {
                  this.handleCreate();
                }
              }}
            />
          </div>
          <div style={{ flex: 1 }}>
            <SelectField
              floatingLabelText="type"
              floatingLabelFixed={true}
              onFocus={() =>
                !this.state.paused && this.setState({ paused: true })
              }
              onChange={(e, _, val) => {
                this.setState({ type: val, paused: false });
              }}
              value={type}
            >
              {KEY_TYPES.map(({ type, name }) => (
                <MenuItem key={type} value={type} primaryText={name} />
              ))}
            </SelectField>
          </div>
        </div>
        <div style={{ display: "flex" }}>
          <div style={{ flex: 1 }}>
            {type !== "enum" &&
              renderField({
                value,
                meta: { "check/type": type },
                debounce: false,
                onChange: (value) => this.setState({ value }),
                onKeyPress: (e) => {
                  if (!nameEmpty && !error && e.key === "Enter") {
                    this.handleCreate();
                  }
                },
                onError: (err) => this.setState({ error: err }),
                label: "value",
              })}
            {type === "enum" && (
              <div
                style={{
                  display: "block",
                  marginTop: 16,
                  color: "rgba(0, 0, 0, 0.5)",
                }}
              >
                <b style={{ fontSize: "1.1em" }}>Please note:</b>
                <br />
                You can only define options after the key is created.
                <br />
                Please create the key, then
                <i style={{ paddingLeft: 6, paddingRight: 8 }}>
                  <ActionBuild
                    style={{
                      width: 14,
                      height: 14,
                      marginRight: 4,
                      color: "rgba(0, 0, 0, 0.5)",
                    }}
                  />
                  configure metadata
                </i>
                to define options.
              </div>
            )}
          </div>
          <div style={{ flex: 1 }}>
            <SelectField
              floatingLabelText="visibility"
              floatingLabelFixed={true}
              onFocus={() =>
                !this.state.paused && this.setState({ paused: true })
              }
              onChange={(e, _, val) => {
                this.setState({ visibility: val, paused: false });
              }}
              value={visibility}
            >
              {Object.keys(VISIBILITY_LEVELS).map((lvl) => (
                <MenuItem key={lvl} value={lvl} primaryText={lvl} />
              ))}
            </SelectField>
          </div>
        </div>
      </FocusTrapDialog>
    );
  }
}
