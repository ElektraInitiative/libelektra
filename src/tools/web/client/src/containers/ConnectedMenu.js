/**
 * @file
 *
 * @brief connect the Menu component to redux
 *
 * by mapping redux state and action creators to its properties
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { connect } from 'react-redux'
import { bindActionCreators } from 'redux'

import Menu from '../components/Menu.jsx'
import { addInstance } from '../actions'

const mapStateToProps = (state) => {
  return {
    loading: !state.idle,
    status: state.container,
    instances: state.instances,
  }
}

const mapDispatchToProps = (dispatch) =>
  bindActionCreators({ addInstance }, dispatch)

export default connect(mapStateToProps, mapDispatchToProps)(Menu)
