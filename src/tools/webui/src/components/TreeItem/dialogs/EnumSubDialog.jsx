/**
 * @file
 *
 * @brief section of the settings dialog to modify enum (radio button) metadata
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from "react";

import TextField from "material-ui/TextField";
import IconButton from "material-ui/IconButton";
import ContentAddIcon from "material-ui/svg-icons/content/add";
import ActionDeleteIcon from "material-ui/svg-icons/action/delete";

import SavedIcon from "../SavedIcon.jsx";
import debounce from "../../debounce";
import { parseEnum } from "../utils";

const DebouncedTextField = debounce(TextField);

class EnumItem extends Component {
  constructor(props, ...args) {
    super(props, ...args);
    this.state = { value: props.value || "" };
  }

  render() {
    const { id, onChange, saved } = this.props;
    const { value } = this.state;
    return (
      <span>
        <DebouncedTextField
          style={{ marginLeft: 8, width: 100 }}
          floatingLabelText={`option ${id}`}
          floatingLabelFixed={true}
          value={value}
          onChange={(value) => this.setState({ value })}
          onDebounced={onChange}
        />
        <SavedIcon saved={saved} />
      </span>
    );
  }
}

export default class EnumSubDialog extends Component {
  constructor(props, ...args) {
    super(props, ...args);
    this.state = { values: parseEnum(props.value) };
  }

  // keep state in sync with property updates
  componentWillReceiveProps(nextProps) {
    this.setState({ values: parseEnum(nextProps.value) });
  }

  handleDelete = () => {
    const { deleteMeta } = this.props;
    const { values } = this.state;
    deleteMeta(values.length - 1);
  };

  handleCreate = () => {
    const { onChange } = this.props;
    const { values } = this.state;
    onChange(values.length)("");
  };

  render() {
    const { onChange, saved } = this.props;
    const { values } = this.state;
    return (
      <div style={{ marginTop: 16 }}>
        <h3>
          Options
          <IconButton
            style={{ width: 24, height: 24, padding: 4 }}
            iconStyle={{ width: 16, height: 16 }}
            tooltip="create new option"
            onClick={this.handleCreate}
          >
            <ContentAddIcon color="#00BCD4" />
          </IconButton>
          <IconButton
            style={{ width: 24, height: 24, padding: 4 }}
            iconStyle={{ width: 16, height: 16 }}
            tooltip="delete last option"
            onClick={this.handleDelete}
          >
            <ActionDeleteIcon />
          </IconButton>
        </h3>
        <div style={{ display: "block" }}>
          {values.map((v, i) => (
            <EnumItem
              key={"option" + i}
              id={i}
              value={v}
              saved={saved(i)}
              onChange={onChange(i)}
            />
          ))}
        </div>
      </div>
    );
  }
}
