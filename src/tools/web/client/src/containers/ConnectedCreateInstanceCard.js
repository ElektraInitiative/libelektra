/**
 * @file
 *
 * @brief connect the CreateInstanceCard component to redux
 *
 * by mapping redux state and action creators to its properties
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

import { connect } from 'react-redux'
import { bindActionCreators } from 'redux'

import CreateInstanceCard from '../components/CreateInstanceCard.jsx'
import { createInstance, unaddInstance } from '../actions'

const mapStateToProps = (state) => {
  return {}
}

const mapDispatchToProps = (dispatch) =>
  bindActionCreators({ createInstance, unaddInstance }, dispatch)

export default connect(mapStateToProps, mapDispatchToProps)(CreateInstanceCard)
