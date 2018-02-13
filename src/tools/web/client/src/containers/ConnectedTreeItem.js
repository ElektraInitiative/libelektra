/**
 * @file
 *
 * @brief connect the TreeItem component to redux
 *
 * by mapping redux state and action creators to its properties
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { connect } from 'react-redux'
import { bindActionCreators } from 'redux'

import TreeItem from '../components/TreeItem'
import {
  setKey, deleteKey, copyKey, sendNotification, setMetaKey, deleteMetaKey
} from '../actions'

const mapStateToProps = (state) => {
  return {}
}

const mapDispatchToProps = (dispatch) =>
  bindActionCreators({
    setKey, deleteKey, copyKey, sendNotification, setMetaKey, deleteMetaKey
  }, dispatch)

export default connect(mapStateToProps, mapDispatchToProps)(TreeItem)
