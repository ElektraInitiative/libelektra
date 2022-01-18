/**
 * @file
 *
 * @brief connect the NotificationSnackbar component to redux
 *
 * by mapping redux state and action creators to its properties
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { connect } from "react-redux";

import NotificationSnackbar from "../components/NotificationSnackbar.jsx";

const mapStateToProps = (state) => {
  return { message: state.notification };
};

const mapDispatchToProps = (dispatch) => {
  return {};
};

export default connect(
  mapStateToProps,
  mapDispatchToProps
)(NotificationSnackbar);
