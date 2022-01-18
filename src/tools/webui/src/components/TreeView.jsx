/**
 * @file
 *
 * @brief interactive tree view to edit configurations of instances
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from "react";
import { ExplorerView } from "bosket-react";

import TreeItem from "../containers/ConnectedTreeItem";
import { visibility, ARRAY_KEY_REGEX } from "../utils";

import "../css/treeview.css";

const NAMESPACES_ORDER = ["spec", "dir", "user", "system"];

export default class TreeView extends React.Component {
  constructor(props, ...args) {
    super(props, ...args);
    this.state = { selection: [], unfolded: [], data: props.data };
  }

  componentWillReceiveProps = (nextProps) => {
    if (this.props.data !== nextProps.data) {
      this.setState({ data: nextProps.data });
    }

    // kdb updated
    if (this.props.kdb !== nextProps.kdb || this.props.ls !== nextProps.ls) {
      // re-render tree view
      this.setState({ data: nextProps.data.slice() });
    }

    const { unfolded } = this.state;
    if (unfolded.length <= 0 || nextProps.searching) {
      const { instance } = nextProps;
      if (instance && instance.unfolded && instance.unfolded.length > 0) {
        this.setState({ unfolded: instance.unfolded });
      }
    }
  };

  updateUnfolded = (unfolded) => {
    const { instanceId, updateInstance, searching } = this.props;
    this.setState({ unfolded });
    if (!searching) {
      updateInstance(instanceId, { unfolded });
    }
  };

  refreshPath = (path) => {
    const { getKey, instanceId } = this.props;
    return getKey(instanceId, path);
  };

  refreshItem = (item, noRecursive = false) => {
    const mainPromise = this.refreshPath(item.path);
    if (!noRecursive && Array.isArray(item.children)) {
      return Promise.all([
        mainPromise,
        ...item.children.map((child) => this.refreshPath(child.path)),
      ]);
    } else {
      return mainPromise;
    }
  };

  handleSelect = (newSelection, item, ancestors, neighbours) => {
    this.setState({ selection: newSelection });
    // push refresh action at the back of the queue (re-renders selection first)
    setTimeout(() => this.refreshItem(item, true));
  };

  handleDrop = (target, evt, inputs) => {
    const { instanceId, moveKey, copyKey, sendNotification } = this.props;
    const { selection } = inputs;

    // alt or ctrl pressed -> copy
    const action = evt && (evt.altKey || evt.ctrlKey) ? copyKey : moveKey;

    const actionName = evt && (evt.altKey || evt.ctrlKey) ? "copied" : "moved";

    Promise.all(
      selection.map((sel) =>
        action(instanceId, sel.path, target.path + "/" + sel.name)
      )
    ).then(() =>
      sendNotification(`successfully ${actionName} key(s) to ${target.path}`)
    );
    this.setState({ selection: [] });
  };

  renderItem = (item, inputs) => {
    const { kdb, instanceId, instanceVisibility } = this.props;
    const data = kdb && kdb[item.path];

    const isRootPath = !item.path.includes("/");

    if (!isRootPath) {
      // namespaces are always shown
      const lvl =
        data && data.meta && data.meta["visibility"]
          ? visibility(data.meta["visibility"])
          : visibility("user"); // default visibility is user

      if (lvl < visibility(instanceVisibility)) {
        // hide this item
        return false;
      }
    }

    return (
      <TreeItem
        data={data}
        item={item}
        inputs={inputs}
        instanceId={instanceId}
        pathExists={(path) => kdb && kdb[path]}
        instanceVisibility={instanceVisibility}
        refreshPath={this.refreshPath}
      />
    );
  };

  handleSearch = (input) => (item) => {
    // check name and path first
    const regex = new RegExp(`.*${input}.*`, "gi");
    if (item.name.match(regex) || item.path.match(regex)) {
      return true;
    }

    // if available, check data too
    const { kdb } = this.props;
    const data = kdb && kdb[item.path];
    if (data && data.value && data.value.match(regex)) {
      return true;
    }

    return false;
  };

  handleSort = (a, b) => {
    if (a.root) {
      // is a namespace -> special ordering
      const aI = NAMESPACES_ORDER.indexOf(a.name);
      const bI = NAMESPACES_ORDER.indexOf(b.name);
      return aI - bI;
    }
    const matchA = a.name.match(ARRAY_KEY_REGEX);
    const matchB = b.name.match(ARRAY_KEY_REGEX);
    if (matchA && matchB) {
      const [, , indexA] = matchA;
      const [, , indexB] = matchB;
      return Number(indexA) - Number(indexB); // compare array key index directly (ignore prefix)
    } else if (!a.children === !b.children) {
      return a.name.localeCompare(b.name);
    }
    return a.children ? -1 : 1; // list keys with subkeys first
  };

  createOpener() {
    const tree = this;
    return class Opener extends React.Component {
      onClick = (event) => {
        const { onClick, item } = this.props;
        const { unfolded } = tree.state;
        const newUnfolded = unfolded.filter((p) => p !== item.path);
        if (newUnfolded.length === unfolded.length) {
          newUnfolded.push(item.path);
        }
        tree.updateUnfolded(newUnfolded);
        onClick(event);
        event.stopPropagation();
      };

      render() {
        const { onClick, item, children, ...rest } = this.props;
        return (
          <span
            {...rest}
            tabIndex="0"
            onClick={this.onClick}
            onKeyPress={(e) => {
              if (e.key === "Enter") {
                this.onClick(e);
              }
            }}
          >
            {children}
          </span>
        );
      }
    };
  }

  render() {
    const { data, selection, unfolded } = this.state;
    const tree = this;
    const strategies = {
      click: [
        function unfoldOnSelectionByPath(item) {
          if (!this.isSelected(item) && item.children) {
            const newUnfolded = unfolded.filter((p) => p !== item.path);
            if (newUnfolded.length === unfolded.length) {
              newUnfolded.push(item.path);
              tree.updateUnfolded(newUnfolded);
              const newVal = this.state
                .get()
                .unfolded.filter((i) => i !== item);
              if (newVal.length === this.state.get().unfolded.length)
                newVal.push(item);
              this.state.set({ unfolded: newVal });
            }
          }
          return this.inputs
            .get()
            .onSelect(
              item,
              this.inputs.get().ancestors,
              this.inputs.get().model
            );
        },
      ],
      fold: [
        function unfoldByPath(item) {
          const isFolded = !unfolded.find((p) => p === item.path);
          if (!isFolded) {
            const newVal = this.state.get().unfolded.filter((i) => i !== item);
            if (newVal.length === this.state.get().unfolded.length)
              newVal.push(item);
            this.state.set({ unfolded: newVal });
          }
          return isFolded;
        },
      ],
    };

    return (
      <ExplorerView
        dragndrop={{
          drop: this.handleDrop,
          droppable: true /* allow dropping to keys without children */,
        }}
        model={data}
        category="children"
        name="name"
        opener={this.createOpener()}
        search={this.handleSearch}
        sort={this.handleSort}
        selection={selection}
        strategies={strategies}
        updateModel={this.handleUpdate}
        onSelect={this.handleSelect}
        display={this.renderItem}
        transition={{
          transitionName: "ExplorerViewTransition",
          transitionEnterTimeout: 200,
          transitionLeaveTimeout: 200,
        }}
        openerOpts={{ position: "left" }}
        labels={{ "search.placeholder": "Filter keys..." }}
      />
    );
  }
}
