/**
 * @file
 *
 * @brief connect the Container component to redux
 *
 * by mapping redux state and action creators to its properties
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

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
