/**
 * @file
 *
 * @brief simple editable text field for tree items
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from "react";

import TextField from "material-ui/TextField";

import validateType from "./validateType";
import debounce from "../../debounce";
import { fromElektraBool, isNumberType } from "../../../utils";

const DebouncedTextField = debounce(TextField, { timeout: 1500 });

export default class SimpleTextField extends Component {
  constructor(props) {
    super(props);
    this.state = { value: props.value || "", error: false };
  }

  componentWillReceiveProps(nextProps) {
    if (nextProps.value !== this.props.value) {
      this.setState({ value: nextProps.value });
    }
  }

  render() {
    const {
      id,
      meta,
      label,
      debounce = true,
      onChange,
      onError,
      onKeyPress,
    } = this.props;
    const val = this.state.value;
    const comp = debounce ? DebouncedTextField : TextField;
    const isBinary = meta && meta.hasOwnProperty("binary");
    const type = (meta && meta["check/type"]) || "any";

    return (
      <div draggable="true" onDragStart={(e) => e.preventDefault()}>
        {React.createElement(comp, {
          id,
          value: val || (isBinary ? "(null)" : ""),
          tabIndex: 0,
          className: "value",
          errorText: this.state.error,
          hintText: meta && meta.example ? `e.g. ${meta.example}` : false,
          onChange: debounce
            ? (value) => this.setState({ value })
            : (evt) =>
                evt && evt.target && evt.target.value
                  ? onChange(evt.target.value)
                  : onChange(""),
          onDebounced:
            debounce &&
            ((currentValue) => {
              const validationError = validateType(meta, currentValue);
              if (validationError) {
                if (typeof onError === "function") onError(validationError);
                return this.setState({ error: validationError });
              } else {
                if (typeof onError === "function") onError(false);
                this.setState({ error: false });
              }
              onChange(currentValue);
            }),
          disabled: isBinary || fromElektraBool(meta && meta["restrict/write"]),
          floatingLabelText: label,
          floatingLabelFixed: !!label,
          onKeyPress: onKeyPress,
          onKeyDown:
            isNumberType(type) &&
            ((e) => {
              if (
                [46, 8, 9, 27, 13, 110, 190].includes(e.keyCode) || // allow backspace, delete, etc
                // allow: ctrl/cmd+A
                (e.keyCode === 65 &&
                  (e.ctrlKey === true || e.metaKey === true)) ||
                // allow: ctrl/cmd+C
                (e.keyCode === 67 &&
                  (e.ctrlKey === true || e.metaKey === true)) ||
                // allow: ctrl/cmd+X
                (e.keyCode === 88 &&
                  (e.ctrlKey === true || e.metaKey === true)) ||
                // allow: home, end, left, right
                (e.keyCode >= 35 && e.keyCode <= 39)
              ) {
                // let it happen, don't do anything
                return;
              }
              // ensure that it is a number and stop the keypress
              if (
                (e.shiftKey || e.keyCode < 48 || e.keyCode > 57) &&
                (e.keyCode < 96 || e.keyCode > 105)
              ) {
                e.preventDefault();
              }
            }),
        })}
      </div>
    );
  }
}
