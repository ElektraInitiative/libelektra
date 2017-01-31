/* containers/ConnectedErrorSnackbar.js
connect the ErrorSnackbar component to redux by mapping redux state and action
creators to its properties
*/

import { connect } from 'react-redux'

import ErrorSnackbar from '../components/ErrorSnackbar.jsx'

const mapStateToProps = (state) => {
  return { error: state.error }
}

const mapDispatchToProps = (dispatch) => {
  return {}
}

export default connect(mapStateToProps, mapDispatchToProps)(ErrorSnackbar)
