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
import { actions as undoActions } from "redux-undo-redo-middleware";

import {
  copyKey,
  createKey,
  deleteKey,
  deleteMetaKey,
  getKey,
  moveKey,
  resetBatchUndo,
  sendNotification,
  setKey,
  setMetaKey
} from "../actions";
import TreeItem from "../components/TreeItem";

const mapStateToProps = state => {
  return { batchUndo: state.batchUndo, kdbState: state.kdb };
};

const mapDispatchToProps = dispatch =>
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
      resetBatchUndo
    },
    dispatch
  );

export default connect(mapStateToProps, mapDispatchToProps)(TreeItem);
