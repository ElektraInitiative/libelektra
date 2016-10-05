import { connect } from 'react-redux'
import { bindActionCreators } from 'redux'

import Configuration from '../components/Configuration.jsx'
import { returnToMain, getKey, setKey } from '../actions'

const mapStateToProps = (state) => {
  return {
    instance: state.instances.filter(instance => instance.id === state.router.id)[0],
    cluster: state.clusters.filter(cluster => cluster.id === state.router.id)[0],
    kdb: state.kdb && state.kdb[state.router.id],
    configuring: state.router.configuring,
  }
}

const mapDispatchToProps = (dispatch) =>
  bindActionCreators({ returnToMain, getKey, setKey }, dispatch)

export default connect(mapStateToProps, mapDispatchToProps)(Configuration)
