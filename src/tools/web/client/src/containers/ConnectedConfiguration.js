/**
 * @file
 *
 * @brief connect the Configuration component to redux
 *
 * by mapping redux state and action creators to its properties
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { connect } from 'react-redux'
import { bindActionCreators } from 'redux'

import Configuration from '../components/Configuration.jsx'
import {
  returnToMain,
  getKey, setKey, deleteKey,
} from '../actions'

const mapStateToProps = (state) => {
  return {
    instance: state.instances.filter(instance => instance.id === state.router.id)[0],
    kdb: state.kdb && state.kdb[state.router.id],
  }
}

const mapDispatchToProps = (dispatch) =>
  bindActionCreators({
    returnToMain,
    getKey,
    setKey,
    deleteKey,
  }, dispatch)

export default connect(mapStateToProps, mapDispatchToProps)(Configuration)
