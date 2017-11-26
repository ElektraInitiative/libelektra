/**
 * @file
 *
 * @brief connect the InstanceCard component to redux
 *
 * by mapping redux state and action creators to its properties
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { connect } from 'react-redux'
import { bindActionCreators } from 'redux'

import InstanceCard from '../components/InstanceCard.jsx'
import {
  updateInstance, deleteInstance, configureInstance, selectInstance,
} from '../actions'

const mapStateToProps = (state, { id }) => {
  return {
    addingCluster: state.container.addingCluster,
    checked: state.container.clusterInstances.indexOf(id) > -1,
  }
}

const mapDispatchToProps = (dispatch) =>
  bindActionCreators({
    updateInstance, deleteInstance, configureInstance, selectInstance,
  }, dispatch)

export default connect(mapStateToProps, mapDispatchToProps)(InstanceCard)
