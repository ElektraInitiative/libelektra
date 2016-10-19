import { connect } from 'react-redux'

import ErrorSnackbar from '../components/ErrorSnackbar.jsx'

const mapStateToProps = (state) => {
  return { error: state.error }
}

const mapDispatchToProps = (dispatch) => {
  return {}
}

export default connect(mapStateToProps, mapDispatchToProps)(ErrorSnackbar)
