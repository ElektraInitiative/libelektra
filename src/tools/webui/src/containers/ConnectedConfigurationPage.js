/**
 * @file
 *
 * @brief connect the Configuration component to redux
 *
 * by mapping redux state and action creators to its properties
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { connect } from "react-redux";
import { bindActionCreators } from "redux";

import Configuration from "../components/pages/Configuration.jsx";
import {
  getKdb,
  getKey,
  setKey,
  deleteKey,
  sendNotification,
} from "../actions";

const mapStateToProps = (state, { match }) => {
  const { id } = match && match.params;
  const { error, instances, ls } = state;
  return {
    instance: instances.find((instance) => instance.id === id),
    ls,
    instanceError: error && error.instanceError ? error : false,
    search: state.kdbFind,
  };
};

const mapDispatchToProps = (dispatch) =>
  bindActionCreators(
    {
      getKdb,
      getKey,
      setKey,
      deleteKey,
      sendNotification,
    },
    dispatch
  );

export default connect(mapStateToProps, mapDispatchToProps)(Configuration);
