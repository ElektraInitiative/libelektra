/**
 * @file
 *
 * @brief radio buttons for tree items
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from "react";

import { RadioButton, RadioButtonGroup } from "material-ui/RadioButton";

import { fromElektraBool } from "../../../utils";

export default class RadioButtons extends Component {
  constructor(props) {
    super(props);
    this.state = { value: props.value || false };
  }

  componentWillReceiveProps(nextProps) {
    this.setState({ value: nextProps.value || false });
  }

  render() {
    const { id, value, meta, options, onChange } = this.props;
    const val = this.state.value === false ? value : this.state.value;

    return (
      <RadioButtonGroup
        id={id}
        name={id}
        valueSelected={val}
        onChange={(evt, value) => onChange(value)}
        style={{
          display: "inline-block",
          position: "relative",
          top: 7,
          marginTop: -11,
        }}
      >
        {options.map((option) => (
          <RadioButton
            tabIndex="0"
            className="value"
            key={id + "-" + option}
            value={option}
            label={option}
            style={{ display: "inline-block", width: "auto", paddingRight: 32 }}
            disabled={
              (meta && meta.hasOwnProperty("binary")) ||
              fromElektraBool(meta && meta["restrict/write"])
            }
          />
        ))}
      </RadioButtonGroup>
    );
  }
}
