import { connect } from 'react-redux'

import Container from '../components/Container.jsx'

const mapStateToProps = (state) => {
  return {
    instances: state.instances,
    status: state.container,
  }
}

const mapDispatchToProps = (dispatch) => {
  return {}
}

export default connect(mapStateToProps, mapDispatchToProps)(Container)
