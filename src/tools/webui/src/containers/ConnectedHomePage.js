/**
 * @file
 *
 * @brief connect the Home page component to redux
 *
 * by mapping redux state and action creators to its properties
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { connect } from "react-redux";

import Home from "../components/pages/Home.jsx";

const mapStateToProps = (state) => {
  return {
    instances: state.instances,
    status: state.container,
  };
};

const mapDispatchToProps = (dispatch) => {
  return {};
};

export default connect(mapStateToProps, mapDispatchToProps)(Home);
