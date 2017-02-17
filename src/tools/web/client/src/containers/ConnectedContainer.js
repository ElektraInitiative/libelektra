/**
 * @file
 *
 * @brief connect the Container component to redux
 *
 * by mapping redux state and action creators to its properties
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

import { connect } from 'react-redux'

import Container from '../components/Container.jsx'

const inCluster = (clusters, instanceId) =>
  clusters.reduce(
    (res, cluster) => res || cluster.instances.indexOf(instanceId) > -1,
    false
  )

const mapStateToProps = (state) => {
  return {
    instances: state.instances.filter(
      (instance) => !inCluster(state.clusters, instance.id)
    ),
    clusters: state.clusters,
    status: state.container,
  }
}

const mapDispatchToProps = (dispatch) => {
  return {}
}

export default connect(mapStateToProps, mapDispatchToProps)(Container)
