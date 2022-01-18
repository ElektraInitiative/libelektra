/**
 * @file
 *
 * @brief utility higher-order component to debounce a handler function
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from "react";

export default function debounce(
  WrappedComponent,
  { timeout = 750, handlerFn = "onChange" } = {}
) {
  return class extends React.Component {
    constructor(...args) {
      super(...args);
      this.state = { timeoutFn: false };
    }

    handleChange = (evt, _, val) => {
      const { timeoutFn } = this.state;
      const { onChange, onDebounced } = this.props;

      const value = val || evt.target.value;

      if (typeof onChange === "function") onChange(value);
      if (timeoutFn) clearTimeout(timeoutFn);

      this.setState({
        timeoutFn: setTimeout(() => {
          this.setState({ timeoutFn: false });
          if (typeof onDebounced === "function") onDebounced(value);
        }, timeout),
      });
    };

    render() {
      // do not pass onDebounced down to the wrapped component
      const { inputRef, onDebounced, ...originalProps } = this.props;
      const injectedProps = {
        [handlerFn]: this.handleChange,
      };
      return (
        <WrappedComponent
          {...originalProps}
          {...injectedProps}
          ref={inputRef}
        />
      );
    }
  };
}
