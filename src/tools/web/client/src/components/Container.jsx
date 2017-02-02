/**
 * @file
 *
 * @brief this is the main overview of all instances and clusters
 *
 * it's a grid container showing clusters first, then instances. it also renders
 * the CreateInstanceCard when the user presses the "create instance" button.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

import React from 'react'

import { Grid, Cell } from 'rgx'

import ConnectedInstanceCard from '../containers/ConnectedInstanceCard'
import ConnectedClusterCard from '../containers/ConnectedClusterCard'
import ConnectedCreateInstanceCard from '../containers/ConnectedCreateInstanceCard'

const containerStyle = {
  minHeight: '500px',
}

const Container = ({ instances, clusters, status }) =>
    <div style={containerStyle}>
        <Grid gutter={5}>
            {clusters && clusters.map(cluster =>
              <Cell key={cluster.id} min={256} max={512}>
                  <ConnectedClusterCard
                    id={cluster.id}
                    name={cluster.name}
                    instances={cluster.instances}
                  />
              </Cell>
            )}
            {instances && instances.map(instance =>
              <Cell key={instance.id} min={256} max={512}>
                  <ConnectedInstanceCard
                    id={instance.id}
                    name={instance.name}
                    host={instance.host}
                  />
              </Cell>
            )}
        </Grid>
        {status && status.addingInstance &&
          <Cell key="addingInstance" min={256} max={512}>
              <ConnectedCreateInstanceCard />
          </Cell>
        }
    </div>

export default Container
