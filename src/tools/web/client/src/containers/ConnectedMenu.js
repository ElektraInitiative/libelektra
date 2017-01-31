/* containers/ConnectedMenu.js
connect the Menu component to redux by mapping redux state and action creators
to its properties
*/

import { connect } from 'react-redux'
import { bindActionCreators } from 'redux'

import Menu from '../components/Menu.jsx'
import { addInstance, addCluster, unaddCluster, returnToMain, createCluster } from '../actions'

const mapStateToProps = (state) => {
  return {
    loading: !state.idle,
    addingCluster: state.container.addingCluster,
    clusterInstances: state.container.clusterInstances,
  }
}

const mapDispatchToProps = (dispatch) =>
  bindActionCreators({ addInstance, addCluster, unaddCluster, returnToMain, createCluster }, dispatch)

export default connect(mapStateToProps, mapDispatchToProps)(Menu)
