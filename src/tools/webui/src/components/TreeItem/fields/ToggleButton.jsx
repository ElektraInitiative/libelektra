/**
 * @file
 *
 * @brief toggle button for tree items
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from "react";

import Checkbox from "material-ui/Checkbox";

import { fromElektraBool } from "../../../utils";

export default class ToggleButton extends React.Component {
  constructor(props, ...args) {
    super(props, ...args);
    this.state = { checked: props.value === "1" };
  }

  componentWillReceiveProps(nextProps) {
    this.setState({ checked: nextProps.value === "1" });
  }

  handleCheck = (evt, checked) => {
    const { onChange } = this.props;
    this.setState({ checked });
    onChange(checked ? "1" : "0");
  };

  render() {
    const { id, meta, label } = this.props;
    return (
      <Checkbox
        className="value"
        tabIndex="0"
        id={id}
        label={(meta && meta.description) || label}
        checked={this.state.checked}
        onCheck={this.handleCheck}
        style={{
          display: "inline-block",
          width: "auto",
          position: "relative",
          top: 6,
          marginTop: -11,
        }}
        disabled={
          (meta && meta.hasOwnProperty("binary")) ||
          fromElektraBool(meta && meta["restrict/write"])
        }
      />
    );
  }
}
