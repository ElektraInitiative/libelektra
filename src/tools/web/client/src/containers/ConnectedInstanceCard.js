import { connect } from 'react-redux'
import { bindActionCreators } from 'redux'

import InstanceCard from '../components/InstanceCard.jsx'
import {
  updateInstance, deleteInstance, configureInstance, selectInstance
} from '../actions'

const mapStateToProps = (state, { id }) => {
  return {
    addingCluster: state.container.addingCluster,
    checked: state.container.clusterInstances.indexOf(id) > -1,
  }
}

const mapDispatchToProps = (dispatch) =>
  bindActionCreators({
    updateInstance, deleteInstance, configureInstance, selectInstance
  }, dispatch)

export default connect(mapStateToProps, mapDispatchToProps)(InstanceCard)
