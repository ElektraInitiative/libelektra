/**
 * @file
 *
 * @brief connect the TreeView component to redux
 *
 * by mapping redux state and action creators to its properties
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { connect } from "react-redux";
import { bindActionCreators } from "redux";

import TreeView from "../components/TreeView.jsx";
import {
  getKey,
  moveKey,
  copyKey,
  updateInstance,
  sendNotification,
} from "../actions";

const mapStateToProps = (state, { instanceId, treeRef }) => {
  return { kdb: state.kdb && state.kdb[instanceId], ref: treeRef };
};

const mapDispatchToProps = (dispatch) =>
  bindActionCreators(
    { getKey, moveKey, copyKey, updateInstance, sendNotification },
    dispatch
  );

export default connect(mapStateToProps, mapDispatchToProps)(TreeView);
