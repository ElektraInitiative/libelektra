/**
 * @file
 *
 * @brief section of the settings dialog to modify number metadata of keys
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from "react";

import TextField from "material-ui/TextField";
import IconButton from "material-ui/IconButton";
import ActionDeleteIcon from "material-ui/svg-icons/action/delete";
import ContentAddIcon from "material-ui/svg-icons/content/add";

import SavedIcon from "../SavedIcon.jsx";
import debounce from "../../debounce";

const DebouncedTextField = debounce(TextField);

class RangeItem extends Component {
  constructor(props, ...args) {
    super(props, ...args);
    const min = props.min || "";
    const max = props.max || "";
    this.state = {
      min,
      max,
      errorMin: min.trim().length <= 0,
      errorMax: max.trim().length <= 0,
    };
  }

  render() {
    const { last, ranges, onChange, onDelete } = this.props;
    const { min, max, errorMin, errorMax } = this.state;
    const onlyItem = ranges.length === 1;

    return (
      <span style={{ marginRight: 32 }}>
        <span style={{ marginRight: 22 }}>
          <DebouncedTextField
            style={{ width: 35, marginRight: 4 }}
            floatingLabelText="min"
            floatingLabelFixed={true}
            value={min}
            underlineStyle={
              errorMin &&
              !onlyItem && { borderBottom: "2px solid rgb(244, 67, 54)" }
            }
            onChange={(value) => this.setState({ min: value })}
            onDebounced={(value) => {
              const { min, max } = this.state; // pull updated values from state
              if (isNaN(min) || min.trim().length <= 0) {
                this.setState({ errorMin: true });
              } else {
                this.setState({ errorMin: false });
                if (!errorMax) onChange([value, max]);
              }
            }}
          />
          {" — "}
          <DebouncedTextField
            style={{ width: 35, marginLeft: 8 }}
            floatingLabelText="max"
            floatingLabelFixed={true}
            value={max}
            underlineStyle={
              errorMax &&
              !onlyItem && { borderBottom: "2px solid rgb(244, 67, 54)" }
            }
            onChange={(value) => this.setState({ max: value })}
            onDebounced={(value) => {
              const { min, max } = this.state; // pull updated values from state
              if (isNaN(max) || max.trim().length <= 0) {
                this.setState({ errorMax: true });
              } else {
                this.setState({ errorMax: false });
                if (!errorMin) onChange([min, value]);
              }
            }}
          />
          <IconButton
            style={{ width: 22, height: 22, padding: 4 }}
            iconStyle={{ width: 14, height: 14 }}
            tooltip="delete range"
            onClick={onDelete}
          >
            <ActionDeleteIcon />
          </IconButton>
        </span>
        {!last && <i>or</i>}
      </span>
    );
  }
}

class Ranges extends Component {
  constructor(props, ...args) {
    super(props, ...args);
    this.state = { ranges: this.parseRanges(props.ranges) || [["", ""]] };
  }

  componentWillReceiveProps(nextProps) {
    this.setState({ ranges: this.parseRanges(nextProps.ranges) || [["", ""]] });
  }

  parseRanges = (rangeStr) => {
    if (!rangeStr) return false;
    try {
      return rangeStr.split(",").map((r) => r.split("-"));
    } catch (err) {
      return false;
    }
  };

  toRangeStr = (ranges) => {
    return ranges.map((r) => r.join("-")).join(",");
  };

  createRange = () => {
    const { ranges } = this.state;
    const newState = [...ranges, ["", ""]];
    this.setState({ ranges: newState });
  };

  render() {
    const { onChange } = this.props;
    const { ranges } = this.state;
    return (
      <div style={{ display: "block" }}>
        {ranges.map(([min, max], i) => (
          <RangeItem
            key={"range" + i}
            min={min}
            max={max}
            last={i === ranges.length - 1}
            ranges={ranges}
            onDelete={() => {
              const newState = ranges.filter((r, j) => i !== j);
              this.setState({
                ranges: newState.length > 0 ? newState : [["", ""]],
              });
              onChange(this.toRangeStr(newState));
            }}
            onChange={(val) => {
              const newState = ranges.map((r, j) => {
                if (i === j) {
                  return val;
                }
                return r;
              });
              this.setState({ ranges: newState });
              onChange(this.toRangeStr(newState));
            }}
          />
        ))}
      </div>
    );
  }
}

export default class NumberSubDialog extends Component {
  handleCreate = () => {
    if (this.ranges) {
      this.ranges.createRange();
    }
  };

  render() {
    const { value, saved, onChange, error } = this.props;

    return (
      <div style={{ marginTop: 16 }}>
        <h3>
          Ranges
          <IconButton
            style={{ marginLeft: 8, top: 2, width: 20, height: 20, padding: 2 }}
            iconStyle={{ width: 16, height: 16 }}
            tooltip="create new range"
            onClick={this.handleCreate}
          >
            <ContentAddIcon color="#00BCD4" />
          </IconButton>
          <SavedIcon saved={saved} />
          {error && (
            <span style={{ color: "red", fontSize: "0.7em" }}>
              Could not save range: Current value does not match range.
            </span>
          )}
        </h3>
        <Ranges
          ref={(r) => (this.ranges = r)}
          ranges={value}
          onChange={onChange}
        />
      </div>
    );
  }
}
