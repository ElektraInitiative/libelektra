/**
 * @file
 *
 * @brief connect the TreeItem component to redux
 *
 * by mapping redux state and action creators to its properties
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { connect } from "react-redux";
import { bindActionCreators } from "redux";

import TreeItem from "../components/TreeItem";
import {
  setKey,
  createKey,
  deleteKey,
  copyKey,
  sendNotification,
  setMetaKey,
  deleteMetaKey,
  resetBatchUndo,
  getKey,
  moveKey,
} from "../actions";
import { actions as undoActions } from "redux-undo-redo-middleware";

const mapStateToProps = (state) => {
  return { batchUndo: state.batchUndo, kdbState: state.kdb };
};

const mapDispatchToProps = (dispatch) =>
  bindActionCreators(
    {
      setKey,
      getKey,
      moveKey,
      createKey,
      deleteKey,
      copyKey,
      sendNotification,
      setMetaKey,
      deleteMetaKey,
      onUndo: undoActions.undo,
      onRedo: undoActions.redo,
      resetBatchUndo,
    },
    dispatch
  );

export default connect(mapStateToProps, mapDispatchToProps)(TreeItem);
