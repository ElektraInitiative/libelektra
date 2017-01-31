/**
 * @file
 *
 * @brief connect the ErrorSnackbar component to redux
 *
 * by mapping redux state and action creators to its properties
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
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
