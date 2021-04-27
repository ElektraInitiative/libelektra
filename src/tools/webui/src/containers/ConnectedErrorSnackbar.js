/**
 * @file
 *
 * @brief connect the ErrorSnackbar component to redux
 *
 * by mapping redux state and action creators to its properties
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { connect } from "react-redux";
import { bindActionCreators } from "redux";

import ErrorSnackbar from "../components/ErrorSnackbar.jsx";
import { dismissError } from "../actions";

const mapStateToProps = (state) => {
  const { error } = state;
  return {
    error:
      error && error.instanceError
        ? false // instance error is already handled by configuration page
        : error,
  };
};

const mapDispatchToProps = (dispatch) =>
  bindActionCreators({ dismissError }, dispatch);

export default connect(mapStateToProps, mapDispatchToProps)(ErrorSnackbar);
