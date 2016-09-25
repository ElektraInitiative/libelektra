import { connect } from 'react-redux'
import { bindActionCreators } from 'redux'

import InstanceCard from '../components/InstanceCard.jsx'
import { updateInstance, deleteInstance, configureInstance } from '../actions'

const mapStateToProps = (state) => {
  return {}
}

const mapDispatchToProps = (dispatch) =>
  bindActionCreators({ updateInstance, deleteInstance, configureInstance }, dispatch)

export default connect(mapStateToProps, mapDispatchToProps)(InstanceCard)
