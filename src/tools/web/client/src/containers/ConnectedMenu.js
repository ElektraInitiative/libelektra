import { connect } from 'react-redux'
import { bindActionCreators } from 'redux'

import Menu from '../components/Menu.jsx'
import { addInstance, addCluster, returnToMain } from '../actions'

const mapStateToProps = (state) => {
  return { loading: !state.idle }
}

const mapDispatchToProps = (dispatch) =>
  bindActionCreators({ addInstance, addCluster, returnToMain }, dispatch)

export default connect(mapStateToProps, mapDispatchToProps)(Menu)
