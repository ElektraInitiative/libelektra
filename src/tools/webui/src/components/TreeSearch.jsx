/**
 * @file
 *
 * @brief search for the tree view
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from "react";
import debounce from "./debounce";

const SearchInput = ({ value, onChange, onKeyPress, state }) => (
  <input
    type="search"
    class={"search " + state}
    placeholder="Search keys..."
    value={value}
    onChange={onChange}
    onKeyPress={onKeyPress}
  />
);
const DebouncedSearchInput = debounce(SearchInput);

export default class TreeSearch extends React.Component {
  constructor(...args) {
    super(...args);
    this.state = { value: "" };
  }

  handleChange = (value) => {
    this.setState({ value });
  };

  handleFind = (value) => {
    const { instanceId, findKey, clearSearch, sendNotification } = this.props;
    if (value && value.length > 0) {
      findKey(instanceId, value).then((res) => {
        if (res && res.type === "FIND_KEY_SUCCESS") {
          return sendNotification("search completed successfully!");
        }
        return sendNotification("error while searching!");
      });
    } else {
      clearSearch();
      setTimeout(() => sendNotification("search cleared!"), 200);
    }
  };

  render() {
    const { search } = this.props;
    const { loading, error } = search;
    const { value } = this.state;
    const state =
      value.length > 0
        ? loading
          ? "loading"
          : error
          ? "error"
          : "success"
        : "empty";
    return (
      <DebouncedSearchInput
        state={state}
        value={value}
        onChange={this.handleChange}
        onDebounced={this.handleFind}
        onKeyPress={(e) => {
          if (e.key === "Enter") {
            this.handleFind(value);
          }
        }}
      />
    );
  }
}
