/**
 * @file
 *
 * @brief this is the configuration page
 *
 * it renders the interactive tree view
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from "react";

import { Card, CardHeader, CardText } from "material-ui/Card";
import IconButton from "material-ui/IconButton";
import NavigationRefresh from "material-ui/svg-icons/navigation/refresh";

import TreeView from "../../containers/ConnectedTreeView";
import TreeSearch from "../../containers/ConnectedTreeSearch";
import InstanceError from "../InstanceError.jsx";

const NAMESPACES = ["user:", "system:", "spec:", "dir:"];

// create tree structure from kdb ls result (list of available keys)
const partsTree = (acc, parts) => {
  if (parts.length <= 0) return acc;

  const part = parts.shift();
  if (!acc[part]) acc[part] = {};
  partsTree(acc[part], parts);
  return acc;
};

const createTree = (ls) =>
  ls.reduce((acc, item) => {
    return partsTree(acc, item.split("/"));
  }, {});

const parseDataSet = (
  getKey,
  sendNotification,
  instanceId,
  tree,
  path,
  parent
) => {
  return Object.keys(tree).map((key) => {
    const newPath = path ? path + "/" + key : key;
    let data = {
      name: key,
      path: newPath,
      root: !path,
      parent: parent,
    };
    const children = parseDataSet(
      getKey,
      sendNotification,
      instanceId,
      tree[key],
      newPath,
      data
    );
    data.children =
      Array.isArray(children) && children.length > 0
        ? (notify = true) => {
            return new Promise((resolve) => {
              getKey(instanceId, newPath, true);
              resolve(children);
            });
          }
        : false;
    return data;
  });
};

const parseData = (getKey, sendNotification, instanceId, ls, kdb) => {
  if (!Array.isArray(ls)) return;
  const tree = createTree(ls);
  return parseDataSet(getKey, sendNotification, instanceId, tree);
};

const getUnfolded = (searchResults) => {
  let unfolded = [];
  for (let r of searchResults) {
    const parts = r.split("/");
    let path = "";
    for (let p of parts) {
      path = path.length === 0 ? p : path + "/" + p;
      if (!unfolded.includes(path)) {
        unfolded.push(path);
      }
    }
  }
  return unfolded;
};

// configuration page
export default class Configuration extends Component {
  constructor(props, ...rest) {
    super(props, ...rest);
    const { getKdb, match } = props;
    const { id } = match && match.params;
    getKdb(id);
    this.state = { data: this.generateData(props) || [] };
  }

  componentWillReceiveProps(nextProps) {
    this.setState({ data: this.generateData(nextProps) || [] });
  }

  updateKey = (data, [keyPath, ...paths], keyData) =>
    Array.isArray(data)
      ? data.map((d) => {
          if (d.name === keyPath) {
            if (paths.length > 0) {
              // recurse deeper
              return {
                ...d,
                children: this.updateKey(d.children, paths, keyData),
              };
            }

            // we found the key, replace data
            return keyData;
          }

          // not the path we want to edit
          return d;
        })
      : data;

  updateData = (keyData, paths) => {
    const { data } = this.state;
    const newData = this.updateKey(data, paths, keyData);
    return this.setState({ data: newData });
  };

  waitForData = () => {
    const { sendNotification } = this.props;
    const { data } = this.state;
    const user = Array.isArray(data) && data.find((d) => d.path === "user");
    if (!user || !user.children) {
      this.timeout = setTimeout(this.waitForData, 100);
    } else {
      this.preload(data).then(() =>
        sendNotification("configuration data loaded!")
      );
    }
  };

  componentDidMount() {
    this.waitForData();
  }

  generateData = ({ ls, match, getKey }) => {
    const { id } = match && match.params;
    const { sendNotification } = this.props;
    return parseData(getKey, sendNotification, id, [...NAMESPACES, ...ls]);
  };

  refresh = () => {
    return window.location.reload();
  };

  preload = async (tree, paths = [], levels = 1) => {
    if (!tree) return await Promise.resolve(tree);
    return await Promise.all(
      tree.map(async (item, i) => {
        let { children } = item;

        if (!children) return item;

        const childItems =
          typeof children === "function"
            ? await children(false) // resolve children if necessary
            : children;
        const newPaths = [...paths, item.name];

        let promises = [
          this.updateData(
            {
              ...item,
              children: childItems,
            },
            newPaths
          ),
        ];

        if (levels > 0) {
          if (NAMESPACES.includes(newPaths.indexOf(0))) {
            promises.push(this.preload(childItems, newPaths, levels - 1));
          }
        }

        return Promise.all(promises);
      })
    );
  };

  render() {
    const { instance, match, instanceError, search } = this.props;
    const { data } = this.state;

    if (instanceError) {
      return (
        <Card>
          <CardHeader
            title={
              <h1>
                <b>404</b> instance not found
              </h1>
            }
          />
          <CardText>
            <InstanceError
              instance={instance}
              error={instanceError}
              refresh={this.refresh}
            />
          </CardText>
        </Card>
      );
    }

    if (!instance) {
      const title = (
        <h1>
          <b>Loading instance...</b> please wait
        </h1>
      );
      return (
        <Card>
          <CardHeader title={title} />
        </Card>
      );
    }

    const { id } = match && match.params;
    const { name, description, host, visibility } = instance;

    const title = (
      <h1>
        <b>{name}</b>
        <IconButton
          className="hoverEffect"
          style={{ marginLeft: 6, width: 28, height: 28, padding: 6 }}
          iconStyle={{ width: 16, height: 16 }}
          onClick={this.refresh}
          tooltip="refresh"
        >
          <NavigationRefresh />
        </IconButton>
      </h1>
    );

    const isSearching = search && search.done;
    const hasResults = search && search.results && search.results.length > 0;
    const searchError = search && search.error;

    const filteredData =
      isSearching && hasResults
        ? this.generateData({ ...this.props, ls: search.results })
        : data;

    const autoUnfold =
      isSearching &&
      hasResults &&
      search.results &&
      search.results.length <= 10;

    const filteredInstance = autoUnfold
      ? { ...instance, unfolded: getUnfolded(search.results) }
      : instance;

    return (
      <Card style={{ padding: "8px 16px" }}>
        <CardHeader
          title={title}
          subtitle={
            <span>
              {description ? description + " — " : ""}
              host: <span style={{ opacity: 0.7 }}>{host}</span>
              &nbsp;— visibility:{" "}
              <span style={{ opacity: 0.7 }}>{visibility}</span>
            </span>
          }
        />
        <CardText>
          {instanceError ? (
            <InstanceError
              instance={instance}
              error={instanceError}
              refresh={this.refresh}
            />
          ) : data && Array.isArray(data) && data.length > 0 ? (
            [
              <TreeSearch instanceId={id} />,
              searchError ? (
                <div
                  style={{
                    fontSize: "1.1em",
                    color: "rgba(0, 0, 0, 0.4)",
                    marginTop: "1.5em",
                    padingLeft: "0.5em",
                  }}
                >
                  <b>{searchError.name}:</b> {searchError.message}
                </div>
              ) : isSearching && !hasResults ? (
                <div
                  style={{
                    fontSize: "1.1em",
                    color: "rgba(0, 0, 0, 0.4)",
                    marginTop: "1.5em",
                    padingLeft: "0.5em",
                  }}
                >
                  No results found for "{search.query}".
                </div>
              ) : (
                <TreeView
                  searching={isSearching || (search && search.clearing)}
                  instance={filteredInstance}
                  instanceId={id}
                  data={filteredData}
                  instanceVisibility={visibility}
                />
              ),
            ]
          ) : (
            <div style={{ fontSize: "1.1em", color: "rgba(0, 0, 0, 0.4)" }}>
              Loading configuration data...
            </div>
          )}
        </CardText>
      </Card>
    );
  }
}
