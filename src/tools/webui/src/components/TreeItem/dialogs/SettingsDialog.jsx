/**
 * @file
 *
 * @brief dialog to modify metadata of keys
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from "react";

import FlatButton from "material-ui/FlatButton";
import TextField from "material-ui/TextField";
import SelectField from "material-ui/SelectField";
import MenuItem from "material-ui/MenuItem";
import Checkbox from "material-ui/Checkbox";
import FocusTrapDialog from "./FocusTrapDialog.jsx";

import SavedIcon from "../SavedIcon.jsx";
import EnumSubDialog from "./EnumSubDialog.jsx";
import NumberSubDialog from "./NumberSubDialog.jsx";
import AdditionalMetakeysSubDialog from "./AdditionalMetakeysSubDialog.jsx";

import debounce from "../../debounce";
import {
  VISIBILITY_LEVELS,
  visibility,
  toElektraBool,
  fromElektraBool,
  isNumberType,
} from "../../../utils";
import { KEY_TYPES } from "./utils";
import validateType, { validateRange } from "../fields/validateType";

const DebouncedTextField = debounce(TextField);

const IMMEDIATE = "IMMEDIATE";
const DEBOUNCED = "DEBOUNCED";

export default class SettingsDialog extends Component {
  constructor(...args) {
    super(...args);
    this.state = {
      regexError: false,
      rangeError: false,
      regexStr: false,
      rangeStr: false,
      defaultError: false,
      defaultStr: false,
      paused: false,
    };
  }

  componentWillReceiveProps(nextProps) {
    const {
      regexError,
      rangeError,
      regexStr,
      rangeStr,
      defaultError,
      defaultStr,
    } = this.state;
    if (regexError) {
      this.handleEdit("check/validation", IMMEDIATE)(regexStr);
      setTimeout(
        () => this.handleEdit("check/validation", DEBOUNCED)(regexStr),
        500
      );
    }
    if (rangeError) {
      this.handleEdit("check/range", IMMEDIATE)(rangeStr);
      setTimeout(
        () => this.handleEdit("check/range", DEBOUNCED)(rangeStr),
        500
      );
    }
    if (defaultError) {
      this.handleEdit("default", IMMEDIATE)(defaultStr);
      setTimeout(() => this.handleEdit("default", DEBOUNCED)(defaultStr), 500);
    }
  }

  ensureRegex = (regexStr, data) => {
    if (regexStr) {
      // ensure key value matches regex
      let validationRegex;
      try {
        validationRegex = new RegExp(regexStr);
      } catch (e) {}
      if (!validationRegex) {
        return this.setState({ regexError: "invalid regex code", regexStr });
      } else if (!validationRegex.test(data)) {
        return this.setState({
          regexError: "current value does not match regex",
          regexStr,
        });
      }
    }
    this.setState({ regexError: false, regexStr: false });
  };

  ensureRange = (rangeStr, data) => {
    if (rangeStr) {
      const err = validateRange(rangeStr, data);
      if (err) {
        return this.setState({ rangeError: err, rangeStr });
      }
    }
    this.setState({ rangeError: false, rangeStr: false });
  };

  ensureDefaultValue = (defaultStr) => {
    const { meta } = this.props;
    if (defaultStr) {
      const err = validateType(meta, defaultStr);
      if (err) {
        return this.setState({ defaultError: err, defaultStr });
      }
    }
    this.setState({ defaultError: false, defaultStr: false });
  };

  handleEdit =
    (key, debounced = false) =>
    (value) => {
      const { data, setMeta, deleteMeta } = this.props;

      if (!debounced || debounced === IMMEDIATE) {
        // set value of field
        this.setState({ [key]: { ...this.state[key], value } });

        if (key === "check/validation") {
          this.ensureRegex(value, data);
        }

        if (key === "check/range") {
          this.ensureRange(value, data);
        }

        if (key === "default") {
          this.ensureDefaultValue(value);
        }
      }

      if (!debounced || debounced === DEBOUNCED) {
        if (key === "check/validation" && this.state.regexError) {
          return; // do not save regex that does not match
        }
        if (key === "check/range" && this.state.rangeError) {
          return; // do not save range that does not match
        }
        if (key === "default" && this.state.defaultError) {
          return; // do not save range that does not match
        }
        // persist value to kdb and show notification
        const { timeout } = this.state[key] || {};

        let action = setMeta;

        // remove metakeys when setting to 0 or empty
        if (key.startsWith("restrict/")) {
          if (value === "0" || value.trim() === "") {
            action = deleteMeta;
          }
        }

        // remove metakeys when setting to default value
        if (key === "check/type" && value === "any") {
          action = deleteMeta;
        }
        if (key === "visibility" && value === "user") {
          action = deleteMeta;
        }

        action(key, value).then(() => {
          if (timeout) clearTimeout(timeout);
          this.setState({
            [key]: {
              ...this.state[key],
              saved: true,
              timeout: setTimeout(() => {
                this.setState({ [key]: { ...this.state[key], saved: false } });
              }, 1500),
            },
          });
        });
      }
    };

  handleBinary = (e, val) => {
    if (val) {
      // convert to binary
      const text =
        "Converting a normal key to a binary key in Elektra Web " +
        "means that you will not be able to convert it back without " +
        "overwriting the current value. Are you sure you want to convert " +
        "this key to a binary key?";
      if (window.confirm(text)) {
        this.handleEdit("binary", IMMEDIATE)(true);
        this.handleEdit("binary", DEBOUNCED)("");
      }
    } else {
      // convert to normal value
      const text =
        "Converting a binary key to a normal key in Elektra Web " +
        "will completely overwrite the current value of the binary key. " +
        "Are you sure you want to convert this key to a normal key and " +
        "wipe its value?";
      if (window.confirm(text)) {
        const { onEdit, deleteMeta, refreshKey } = this.props;
        onEdit("") // wipe value
          .then(() => deleteMeta("binary")) // turn into normal key
          .then(() => {
            refreshKey();
            const { timeout } = this.state["binary"] || {};
            if (timeout) clearTimeout(timeout);
            this.setState({
              binary: {
                ...this.state.binary,
                value: false,
                saved: true,
                timeout: setTimeout(() => {
                  this.setState({
                    binary: { ...this.state.binary, saved: false },
                  });
                }, 1500),
              },
            });
          });
      }
    }
  };

  getMeta(key, fallback) {
    const stateVal = this.state[key] && this.state[key].value;

    const { meta } = this.props;
    const val = meta // meta exists
      ? meta[key]
      : false; // does not exist

    return stateVal === undefined ? val || fallback : stateVal;
  }

  getSaved(key) {
    return this.state[key] && this.state[key].saved;
  }

  renderEnum() {
    return (
      <EnumSubDialog
        onChange={(i) => this.handleEdit(`check/enum/#${i}`)}
        value={(i) => this.props.meta && this.props.meta[`check/enum/#${i}`]}
        saved={(i) => this.getSaved(`check/enum/#${i}`)}
        deleteMeta={(i) => this.props.deleteMeta(`check/enum/#${i}`)}
      />
    );
  }

  renderNumber() {
    return (
      <NumberSubDialog
        onChange={this.handleEdit("check/range")}
        error={this.state.rangeError}
        value={this.getMeta("check/range", "")}
        saved={this.getSaved("check/range")}
      />
    );
  }

  handleVisibilityChange = (val) => {
    const { instanceVisibility } = this.props;
    if (val === this.getMeta("visibility", "user")) {
      // visibility was not changed, ignore
      return;
    }
    if (visibility(val) < visibility(instanceVisibility)) {
      const confirmed = window.confirm(
        "Setting the visibility lower than the instance visibility will hide " +
          "this item in this instance. Only proceed if you no longer plan on " +
          "editing this item here."
      );
      if (!confirmed) return;
    }
    return this.handleEdit("visibility")(val);
  };

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
    const { item, open, meta, field, onClose, onEdit } = this.props;
    const { regexError } = this.state;
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

    const type = this.getMeta("check/type", "any");
    const visibility = this.getMeta("visibility", "user");

    const isBinary = this.state["binary"]
      ? this.state["binary"].value
      : meta && meta.hasOwnProperty("binary");

    return (
      <FocusTrapDialog
        actions={actions}
        modal={false}
        open={open}
        paused={this.state.paused}
        onRequestClose={onClose}
        autoScrollBodyContent={true}
      >
        <h1>
          Metadata for <b>{path}</b>
        </h1>
        <div style={{ display: "flex" }}>
          <div style={{ flex: 1 }}>
            <DebouncedTextField
              floatingLabelText="description"
              floatingLabelFixed={true}
              tabIndex="0"
              hintText="e.g. username of the account"
              onChange={this.handleEdit("description", IMMEDIATE)}
              onDebounced={this.handleEdit("description", DEBOUNCED)}
              value={this.getMeta("description", "")}
              onKeyPress={(e) => {
                if (e.key === "Enter") {
                  onClose();
                }
              }}
            />
            <SavedIcon saved={this.getSaved("description")} />
          </div>
          <div style={{ flex: 1 }}>
            <SelectField
              floatingLabelText="visibility"
              floatingLabelFixed={true}
              onFocus={() =>
                !this.state.paused && this.setState({ paused: true })
              }
              onChange={(e, _, val) => {
                this.handleVisibilityChange(val);
                this.setState({ paused: false });
              }}
              value={visibility}
              onKeyPress={(e) => {
                if (e.key === "Enter") {
                  onClose();
                }
              }}
            >
              {Object.keys(VISIBILITY_LEVELS).map((lvl) => (
                <MenuItem key={lvl} value={lvl} primaryText={lvl} />
              ))}
            </SelectField>
            <SavedIcon
              saved={this.getSaved("visibility")}
              style={{ paddingBottom: 16 }}
            />
          </div>
        </div>
        <div style={{ display: "flex" }}>
          <div style={{ flex: 1 }}>
            <DebouncedTextField
              floatingLabelText="example"
              floatingLabelFixed={true}
              tabIndex="0"
              hintText="e.g. hitchhiker42"
              onChange={this.handleEdit("example", IMMEDIATE)}
              onDebounced={this.handleEdit("example", DEBOUNCED)}
              value={this.getMeta("example", "")}
              onKeyPress={(e) => {
                if (e.key === "Enter") {
                  onClose();
                }
              }}
            />
            <SavedIcon saved={this.getSaved("example")} />
          </div>
          <div style={{ flex: 1 }}>
            <DebouncedTextField
              floatingLabelText="default value"
              floatingLabelFixed={true}
              tabIndex="0"
              errorText={this.state.defaultError}
              onChange={this.handleEdit("default", IMMEDIATE)}
              onDebounced={this.handleEdit("default", DEBOUNCED)}
              value={this.getMeta("default", "")}
              onKeyPress={(e) => {
                if (e.key === "Enter") {
                  onClose();
                }
              }}
            />
            <SavedIcon saved={this.getSaved("default")} />
          </div>
        </div>
        <h2 style={{ marginTop: 48 }}>Type</h2>
        <div style={{ display: "flex", paddingTop: 12 }}>
          <div style={{ flex: 1 }}>
            <Checkbox
              tabIndex="0"
              checked={!!isBinary}
              onCheck={this.handleBinary}
              label="binary"
              disabled={this.getMeta("restrict/binary", "0") === "1"}
              onKeyPress={(e) => {
                if (e.key === "Enter") {
                  onClose();
                }
              }}
            />
            <SavedIcon saved={this.getSaved("binary")} />
          </div>
          <div style={{ flex: 1 }}>
            <Checkbox
              tabIndex="0"
              checked={fromElektraBool(this.getMeta("restrict/binary", false))}
              onCheck={(e, val) =>
                this.handleEdit("restrict/binary")(toElektraBool(val))
              }
              label="restrict/binary"
              disabled={!!isBinary}
              onKeyPress={(e) => {
                if (e.key === "Enter") {
                  onClose();
                }
              }}
            />
            <SavedIcon saved={this.getSaved("restrict/binary")} />
          </div>
        </div>
        <div style={{ display: "flex" }}>
          <div style={{ flex: 1 }}>
            <Checkbox
              tabIndex="0"
              checked={fromElektraBool(this.getMeta("restrict/write", false))}
              onCheck={(e, val) =>
                this.handleEdit("restrict/write")(toElektraBool(val))
              }
              label="restrict/write"
              onKeyPress={(e) => {
                if (e.key === "Enter") {
                  onClose();
                }
              }}
            />
            <SavedIcon saved={this.getSaved("restrict/write")} />
          </div>
          <div style={{ flex: 1 }}>
            <Checkbox
              tabIndex="0"
              checked={fromElektraBool(this.getMeta("restrict/remove", false))}
              onCheck={(e, val) =>
                this.handleEdit("restrict/remove")(toElektraBool(val))
              }
              label="restrict/remove"
              onKeyPress={(e) => {
                if (e.key === "Enter") {
                  onClose();
                }
              }}
            />
            <SavedIcon saved={this.getSaved("restrict/remove")} />
          </div>
        </div>
        {!isBinary && (
          <div>
            <div style={{ display: "flex", alignItems: "center" }}>
              <div style={{ flex: 1 }}>
                <SelectField
                  floatingLabelText="type"
                  floatingLabelFixed={true}
                  onFocus={() =>
                    !this.state.paused && this.setState({ paused: true })
                  }
                  onChange={(e, _, val) => {
                    this.setState({ paused: false });
                    if (val === type) {
                      // type was not changed, ignore
                      return;
                    }
                    if (val === "any" || val === "string") {
                      // changing to `any` or `string` is fine
                      return this.handleEdit("check/type")(val);
                    }

                    const text =
                      "Changing the type from '" +
                      type +
                      "'" +
                      " to '" +
                      val +
                      "' will result in the " +
                      "current value getting overwritten! " +
                      "Are you sure you want to proceed?";
                    if (window.confirm(text)) {
                      this.handleEdit("check/type")(val);
                      setTimeout(() => {
                        if (val === "boolean") onEdit("0");
                        else onEdit("");
                      }, 250);
                    }
                  }}
                  value={type}
                  onKeyPress={(e) => {
                    if (e.key === "Enter") {
                      onClose();
                    }
                  }}
                >
                  {KEY_TYPES.map(({ type, name }) => (
                    <MenuItem key={type} value={type} primaryText={name} />
                  ))}
                </SelectField>
                <SavedIcon
                  saved={this.getSaved("check/type")}
                  style={{ paddingBottom: 16 }}
                />
              </div>
              <div style={{ flex: 1 }}>
                {(type === "string" ||
                  type === "any" ||
                  isNumberType(type)) && (
                  <div>
                    <div>
                      <small>Current value:</small>
                    </div>
                    <div>{field}</div>
                  </div>
                )}
              </div>
            </div>
            {this.getMeta("check/type", false) === "enum"
              ? this.renderEnum()
              : null}
            {isNumberType(this.getMeta("check/type", false))
              ? this.renderNumber()
              : null}
            {(type === "string" || type === "any") && (
              <div style={{ display: "flex" }}>
                <div style={{ flex: 1 }}>
                  <DebouncedTextField
                    floatingLabelText="validation regex"
                    floatingLabelFixed={true}
                    tabIndex="0"
                    hintText="e.g. ^[a-zA-Z0-9]+$"
                    errorText={regexError}
                    onChange={this.handleEdit("check/validation", IMMEDIATE)}
                    onDebounced={this.handleEdit("check/validation", DEBOUNCED)}
                    value={this.getMeta("check/validation", "")}
                    onKeyPress={(e) => {
                      if (e.key === "Enter") {
                        onClose();
                      }
                    }}
                  />
                  <SavedIcon saved={this.getSaved("check/validation")} />
                </div>
                <div style={{ flex: 1 }}>
                  <DebouncedTextField
                    floatingLabelText="validation error message"
                    floatingLabelFixed={true}
                    tabIndex="0"
                    hintText="e.g. invalid username"
                    onChange={this.handleEdit(
                      "check/validation/message",
                      IMMEDIATE
                    )}
                    onDebounced={this.handleEdit(
                      "check/validation/message",
                      DEBOUNCED
                    )}
                    value={this.getMeta("check/validation/message", "")}
                    onKeyPress={(e) => {
                      if (e.key === "Enter") {
                        onClose();
                      }
                    }}
                  />
                  <SavedIcon
                    saved={this.getSaved("check/validation/message")}
                  />
                </div>
              </div>
            )}
          </div>
        )}
        <AdditionalMetakeysSubDialog
          handleEdit={this.handleEdit.bind(this)}
          getMeta={this.getMeta.bind(this)}
          getSaved={this.getSaved.bind(this)}
          meta={this.props.meta}
          deleteMeta={this.props.deleteMeta}
        />
      </FocusTrapDialog>
    );
  }
}
