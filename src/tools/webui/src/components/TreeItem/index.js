/**
 * @file
 *
 * @brief interactive tree view item to edit configurations of instances
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from "react";

import ActionDelete from "material-ui/svg-icons/action/delete";
import ActionBuild from "material-ui/svg-icons/action/build";
import ContentAdd from "material-ui/svg-icons/content/add";
import ContentCopy from "material-ui/svg-icons/content/content-copy";
import ContentEdit from "material-ui/svg-icons/editor/mode-edit";
import ContentPaste from "material-ui/svg-icons/content/content-paste";
import { CopyToClipboard } from "react-copy-to-clipboard";

import ActionButton from "./ActionButton.jsx";
import ArrayIcon from "./ArrayIcon.jsx";
import SavedIcon from "./SavedIcon.jsx";
import SimpleTextField from "./fields/SimpleTextField.jsx";
import RadioButtons from "./fields/RadioButtons.jsx";
import ToggleButton from "./fields/ToggleButton.jsx";
import AddDialog from "./dialogs/AddDialog.jsx";
import SettingsDialog from "./dialogs/SettingsDialog.jsx";
import DuplicateDialog from "./dialogs/DuplicateDialog.jsx";
import EditDialog from "./dialogs/EditDialog.jsx";
import RenameDialog from "./dialogs/RenameDialog.jsx";
import { parseEnum } from "./utils";
import { ARRAY_KEY_REGEX, prettyPrintArrayIndex } from "../../utils";

const getParentPath = (path) => {
  const pp = path.split("/");
  pp.pop();
  return pp.join("/");
};

export default class TreeItem extends Component {
  constructor(...args) {
    super(...args);
    this.state = {
      dialogs: {
        add: false,
        edit: false,
        settings: false,
        remove: false,
        rename: false,
      },
      saved: false,
      err: false,
      savedTimeout: false,
    };
  }

  handleOpen = (dialog) => (e) => {
    e.stopPropagation();
    const { dialogs } = this.state;
    this.props.resetBatchUndo();
    this.setState({ dialogs: { ...dialogs, [dialog]: true } });
  };

  handleClose = (dialog) => () => {
    const { dialogs } = this.state;
    this.setState({ dialogs: { ...dialogs, [dialog]: false } });
  };

  handleDelete = (item) => {
    const { instanceId, deleteKey, setMetaKey, sendNotification, kdbState } =
      this.props;

    if (item && item.parent) {
      const arrayKeyLength = this.getArrayKeyLength(item.parent);
      if (arrayKeyLength && arrayKeyLength > 0) {
        setMetaKey(
          instanceId,
          item.parent.path,
          "array",
          String(arrayKeyLength - 1)
        );
      }
    }

    deleteKey(instanceId, item.path, kdbState && kdbState[instanceId])
      .then(() => {
        if (Array.isArray(item.children) && item.children.length > 0) {
          return Promise.all(
            item.children.map((child) =>
              deleteKey(
                instanceId,
                child.path,
                kdbState && kdbState[instanceId]
              )
            )
          );
        }
      })
      .then(() => sendNotification("successfully deleted key: " + item.path));
  };

  handleAdd = (path, addKeyName, addKeyValue) => {
    const { instanceId, createKey, sendNotification, kdbState } = this.props;
    const fullPath = path + "/" + addKeyName;
    createKey(
      instanceId,
      fullPath,
      addKeyValue,
      kdbState && kdbState[instanceId]
    ).then(() => sendNotification("successfully created key: " + fullPath));
  };

  handleDuplicate = (from, to) => {
    const { instanceId, copyKey, sendNotification } = this.props;
    copyKey(instanceId, from, to).then(() =>
      sendNotification("successfully duplicated key: " + from + " -> " + to)
    );
  };

  handleEdit = (value) => {
    const { savedTimeout } = this.state;
    const { instanceId, setKey, item } = this.props;
    const { path } = item;
    return setKey(instanceId, path, value).then((res) => {
      if (res && res.error) {
        return this.setState({ err: true });
      }
      if (savedTimeout) clearTimeout(savedTimeout);
      this.setState({
        err: false,
        saved: true,
        savedTimeout: setTimeout(() => {
          this.setState({ saved: false });
        }, 1500),
      });
    });
  };

  renderSpecialValue = (id, { value, meta, onChange, label }) => {
    if (meta["check/type"]) {
      if (meta["check/type"] === "enum") {
        const valueFn = (i) => {
          return meta[`check/enum/#${i}`];
        };
        const options = parseEnum(valueFn);
        return (
          <RadioButtons
            id={id}
            value={value}
            meta={meta}
            options={options}
            onChange={onChange || this.handleEdit}
          />
        );
      }

      if (meta["check/type"] === "boolean") {
        return (
          <ToggleButton
            label={label}
            id={id}
            value={value}
            meta={meta}
            onChange={onChange || this.handleEdit}
          />
        );
      }
    }
  };

  renderValue = (
    id,
    { value, meta, debounce = true, onChange, onKeyPress, onError, label }
  ) => {
    const val = typeof value !== "undefined" ? value : meta && meta["default"];

    if (meta) {
      const special = this.renderSpecialValue(id, {
        value: val,
        meta,
        onChange,
        label,
      });
      if (special) return special;
    }

    // fallback
    return (
      <SimpleTextField
        debounce={debounce}
        label={label}
        id={id}
        value={val}
        meta={meta}
        onError={onError}
        onChange={onChange || this.handleEdit}
        onKeyPress={onKeyPress}
      />
    );
  };

  getArrayKeyLength(item) {
    if (!item || !item.path) return false;
    const { kdbState, instanceId } = this.props;
    const data = kdbState[instanceId];
    if (data && data[item.path] && data[item.path].meta) {
      const meta = data[item.path].meta;
      if (meta && meta["array"] && meta["array"] >= 0) {
        return Number(meta["array"]);
      }
    }
    return item && Array.isArray(item.children)
      ? item.children.reduce((res, i) => {
          if (res === false) return false;
          if (!i.name.match(ARRAY_KEY_REGEX)) return false;
          return res + 1;
        }, 0)
      : false;
  }

  keyExists = (path, name) => {
    const { instanceId, getKey } = this.props;
    return getKey(instanceId, path + "/" + name).then(
      (res) => res && res.result
    );
  };

  render() {
    const {
      data,
      item,
      instanceId,
      instanceVisibility,
      batchUndo,
      onUndo,
      onRedo,
      setMetaKey,
      deleteMetaKey,
      sendNotification,
      refreshPath,
      moveKey,
    } = this.props;

    const rootLevel = item && item.path ? !item.path.includes("/") : false;

    const titleStyle = { marginTop: -3, display: "flex", alignItems: "center" };

    const meta = data && data.meta;
    const isCheckbox =
      meta && meta["check/type"] && meta["check/type"] === "boolean";
    // we return no value property if the key doesn't exist, otherwise we return an *empty* value
    const keyExists = rootLevel || (data && data.exists);

    const arrayKeyLength = this.getArrayKeyLength(item);
    const parentArrayKeyLength = this.getArrayKeyLength(item.parent);

    const valueVisible =
      !rootLevel && data && !item.children && arrayKeyLength === false;

    const renderedField = (
      <div style={{ display: "flex", alignItems: "center" }}>
        <div style={{ flex: "initial" }}>
          {this.renderValue(item.path, data || {})}
        </div>
        <div style={{ flex: "initial" }}>
          <SavedIcon saved={this.state.saved} err={this.state.err} />
        </div>
      </div>
    );

    let onClickHandler = undefined;
    if (meta && meta["restrict/write"] === "1") {
      onClickHandler = () =>
        alert("This key is set to read-only and cannot be edited.");
    }
    if (meta && meta.hasOwnProperty("binary")) {
      onClickHandler = () =>
        alert(
          "Elektra Web currently does not support editing binary keys. " +
            "Configure metadata of this key to remove the binary flag."
        );
    }

    // make namespaces a bit larger
    const rootStyle = rootLevel ? { fontSize: "medium" } : {};

    return (
      <a style={{ display: "flex", alignItems: "center", ...rootStyle }}>
        {valueVisible ? (
          <span
            style={{
              display: "flex",
              alignItems: "center",
              height: 48,
              opacity: keyExists ? 1 : 0.4,
            }}
          >
            <b style={titleStyle} onClick={this.handleOpen("rename")}>
              {prettyPrintArrayIndex(item.name)}:{" "}
            </b>
            <span style={{ marginLeft: 6 }} onClick={onClickHandler}>
              {this.renderValue(item.path, data)}
            </span>
          </span>
        ) : (
          <b
            style={{ ...titleStyle, opacity: keyExists ? 1 : 0.4 }}
            onClick={this.handleOpen("rename")}
          >
            <span style={{ flex: "initial", marginTop: -2 }}>
              {prettyPrintArrayIndex(item.name)}
            </span>
            {arrayKeyLength !== false && (
              <span style={{ flex: "initial", marginLeft: 8 }}>
                <ArrayIcon />
              </span>
            )}
          </b>
        )}
        <span className="actions">
          <SavedIcon saved={this.state.saved} err={this.state.err} />
          {!valueVisible && data && data.value && <span>{data.value}</span>}
          {valueVisible && (
            <CopyToClipboard
              text={(data && data.value) || ""}
              onCopy={() =>
                sendNotification(
                  "Copied value of " + item.path + " to clipboard!"
                )
              }
            >
              <ActionButton icon={<ContentPaste />} tooltip="copy value" />
            </CopyToClipboard>
          )}
          <ActionButton
            icon={<ContentAdd />}
            onClick={this.handleOpen("add")}
            tooltip="create sub-key"
          />
          <AddDialog
            item={item}
            arrayKeyLength={arrayKeyLength}
            instanceVisibility={instanceVisibility}
            open={this.state.dialogs.add}
            onAdd={this.handleAdd}
            onClose={this.handleClose("add")}
            renderField={({
              value,
              meta,
              debounce,
              onChange,
              onKeyPress,
              label,
              onError,
            }) =>
              this.renderValue("addValueField", {
                value,
                meta,
                debounce,
                onChange,
                onKeyPress,
                label,
                onError,
              })
            }
            keyExists={this.keyExists}
            setMetaByPath={(path, key, value) =>
              setMetaKey(instanceId, path, key, value)
            }
          />
          {!rootLevel &&
            !valueVisible &&
            !(meta && meta["restrict/write"] === "1") && (
              <ActionButton
                icon={<ContentEdit />}
                onClick={this.handleOpen("edit")}
                tooltip="edit value"
              />
            )}
          <RenameDialog
            item={item}
            open={this.state.dialogs.rename}
            onRename={(name) =>
              moveKey(
                instanceId,
                item.path,
                getParentPath(item.path) + "/" + name
              )
            }
            onClose={this.handleClose("rename")}
          />
          <EditDialog
            renderField={({ onKeyPress }) => (
              <div style={{ display: "flex", alignItems: "center" }}>
                <div style={{ flex: "initial" }}>
                  {this.renderValue("editValueField", { ...data, onKeyPress })}
                </div>
                <div style={{ flex: "initial" }}>
                  <SavedIcon saved={this.state.saved} err={this.state.err} />
                </div>
              </div>
            )}
            item={item}
            meta={meta}
            value={data && data.value}
            open={this.state.dialogs.edit}
            onEdit={this.handleEdit}
            onClose={this.handleClose("edit")}
            refreshKey={() => refreshPath(item.path)}
            sendNotification={sendNotification}
            batchUndo={batchUndo}
            onUndo={onUndo}
            onRedo={onRedo}
          />
          {!rootLevel && (
            <ActionButton
              icon={<ContentCopy />}
              onClick={this.handleOpen("duplicate")}
              tooltip="duplicate key"
            />
          )}
          <DuplicateDialog
            item={item}
            arrayKeyLength={parentArrayKeyLength}
            open={this.state.dialogs.duplicate}
            onDuplicate={this.handleDuplicate}
            onClose={this.handleClose("duplicate")}
            pathExists={this.props.pathExists}
          />
          {!rootLevel && (
            <ActionButton
              icon={<ActionBuild />}
              onClick={this.handleOpen("settings")}
              size={13}
              tooltip="configure metadata"
            />
          )}
          <SettingsDialog
            field={renderedField}
            item={item}
            sendNotification={sendNotification}
            batchUndo={batchUndo}
            onUndo={onUndo}
            onRedo={onRedo}
            meta={data && data.meta}
            data={data && data.value}
            open={this.state.dialogs.settings}
            setMeta={(key, value) =>
              setMetaKey(instanceId, item.path, key, value)
            }
            deleteMeta={(key) => deleteMetaKey(instanceId, item.path, key)}
            onClose={this.handleClose("settings")}
            onEdit={this.handleEdit}
            instanceVisibility={instanceVisibility}
            refreshKey={() => refreshPath(item.path)}
          />
          {!(meta && meta["restrict/remove"] === "1") && (
            <ActionButton
              icon={<ActionDelete />}
              onClick={(e) => {
                this.handleDelete(item);
                e.preventDefault();
              }}
              tooltip="delete key"
            />
          )}
          <i>{!isCheckbox && meta && meta.description}</i>
        </span>
      </a>
    );
  }
}
