/**
 * @file
 *
 * @brief section of the settings dialog where users can add additional metakeys
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from "react";

import TextField from "material-ui/TextField";
import FlatButton from "material-ui/FlatButton";
import IconButton from "material-ui/IconButton";
import ContentAddIcon from "material-ui/svg-icons/content/add";
import ActionDeleteIcon from "material-ui/svg-icons/action/delete";

import SavedIcon from "../SavedIcon.jsx";
import debounce from "../../debounce";
import { HANDLED_METADATA } from "./utils";

const DebouncedTextField = debounce(TextField);

const IMMEDIATE = "IMMEDIATE";
const DEBOUNCED = "DEBOUNCED";

export default class AdditionalMetakeysSubDialog extends Component {
  constructor(props, ...args) {
    super(props, ...args);
    this.state = {
      items: props && props.meta ? this.parseMetadata(props.meta) || [] : [],
    };
  }

  // keep state in sync with property updates
  componentWillReceiveProps(nextProps) {
    if (nextProps && nextProps.meta) {
      this.setState({ items: this.parseMetadata(nextProps.meta) || [] });
    }
  }

  // filter metakeys that are not handled otherwise and convert them
  // to a key/value object format
  parseMetadata = (meta) => {
    const keys = Object.keys(meta)
      .filter((k) => !HANDLED_METADATA.find((m) => k.startsWith(m)))
      .filter((k) => meta[k] !== undefined);
    return keys.map((k) => {
      return { key: k, value: meta[k] };
    });
  };

  // update value of a metakey
  updateValue = (key) => (value) => {
    const { items } = this.state;
    this.setState({
      items: items.map((item) => {
        if (item.key === key) {
          return { key, value };
        }
        return item;
      }),
    });
  };

  // delete a metakey
  deleteItem = (item) => {
    if (
      window.confirm(
        "Do you really want to delete the '" + item.key + "' metakey?"
      )
    ) {
      const { deleteMeta } = this.props;
      deleteMeta(item.key);
    }
  };

  renderItems = () => {
    const { handleEdit, getMeta, getSaved } = this.props;
    const { items } = this.state;

    return items.map((item, index) => (
      <span key={index} style={{ marginRight: 60 }}>
        <DebouncedTextField
          floatingLabelText={item.key}
          floatingLabelFixed={true}
          tabIndex="0"
          value={getMeta(item.key)}
          onChange={handleEdit(item.key, IMMEDIATE)}
          onDebounced={handleEdit(item.key, DEBOUNCED)}
        />
        <SavedIcon saved={getSaved(item.key)} />
        <IconButton
          tabIndex="0"
          style={{ width: 24, height: 24, padding: 4 }}
          iconStyle={{ width: 16, height: 16 }}
          onClick={() => this.deleteItem(item)}
        >
          <ActionDeleteIcon />
        </IconButton>
      </span>
    ));
  };

  createKey = () => {
    const name = prompt("Please enter the key name (e.g. check/condition)");

    if (!name || !name.length || name.trim().length <= 0) {
      return alert("Empty/invalid metakey name.");
    }

    if (HANDLED_METADATA.find((m) => name.startsWith(m))) {
      return alert(
        "Cannot add metakey '" +
          name +
          "' because it is already handled by elektra-web. " +
          "Please use the existing field on the settings page to configure this metakey."
      );
    }

    if (this.state.items.find((item) => name === item.key)) {
      return alert(
        "Cannot add metakey '" +
          name +
          "' because it already exists. Please choose a different name."
      );
    }

    const { handleEdit } = this.props;
    return handleEdit(name)("");
  };

  render() {
    const { items } = this.state;
    const renderedItems =
      items && Array.isArray(items) && items.length > 0
        ? this.renderItems()
        : [
            <div
              key={"no-data"}
              style={{
                fontSize: "1.1em",
                color: "rgba(0, 0, 0, 0.4)",
                marginTop: 16,
              }}
            >
              No additional metadata defined yet.
            </div>,
          ];

    return [
      <h2
        key={"header"}
        style={{ marginTop: 48, marginBottom: 0, display: "block" }}
      >
        Additional Metadata
        <FlatButton
          tabIndex="0"
          label="create new metakey"
          icon={<ContentAddIcon />}
          primary
          style={{ marginLeft: 16 }}
          onClick={this.createKey}
        />
      </h2>,
      ...renderedItems,
    ];
  }
}
