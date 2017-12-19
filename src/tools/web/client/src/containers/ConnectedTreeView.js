/**
 * @file
 *
 * @brief connect the TreeView component to redux
 *
 * by mapping redux state and action creators to its properties
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { connect } from 'react-redux'
import { bindActionCreators } from 'redux'

import TreeView from '../components/TreeView.jsx'
import { getKey } from '../actions'

const mapStateToProps = (state, { instanceId }) => {
  return { kdb: state.kdb && state.kdb[instanceId] }
}

const mapDispatchToProps = (dispatch) =>
  bindActionCreators({ getKey }, dispatch)

export default connect(mapStateToProps, mapDispatchToProps)(TreeView)
