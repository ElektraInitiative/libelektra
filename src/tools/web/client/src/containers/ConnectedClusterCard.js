import { connect } from 'react-redux'
import { bindActionCreators } from 'redux'

import ClusterCard from '../components/ClusterCard.jsx'
import { updateCluster, deleteCluster, configureCluster } from '../actions'

const mapStateToProps = (state) => {
  return {}
}

const mapDispatchToProps = (dispatch) =>
  bindActionCreators({ updateCluster, deleteCluster, configureCluster }, dispatch)

export default connect(mapStateToProps, mapDispatchToProps)(ClusterCard)
