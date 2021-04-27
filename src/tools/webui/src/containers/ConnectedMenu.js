/**
 * @file
 *
 * @brief connect the Menu component to redux
 *
 * by mapping redux state and action creators to its properties
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { connect } from "react-redux";
import { bindActionCreators } from "redux";
import { actions as undoActions } from "redux-undo-redo-middleware";

import Menu from "../components/Menu.jsx";
import { addInstance } from "../actions";

const mapStateToProps = (state) => {
  return {
    loading: !state.idle,
    status: state.container,
    instances: state.instances,
    canUndo: state.undoHistory.undoQueue.length > 0,
    canRedo: state.undoHistory.redoQueue.length > 0,
  };
};

const mapDispatchToProps = (dispatch) =>
  bindActionCreators(
    {
      addInstance,
      onUndo: undoActions.undo,
      onRedo: undoActions.redo,
    },
    dispatch
  );

export default connect(mapStateToProps, mapDispatchToProps)(Menu);
