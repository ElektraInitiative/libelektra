import React from 'react'

import { Grid, Cell } from 'rgx'

import ConnectedInstanceCard from '../containers/ConnectedInstanceCard'
import ConnectedCreateInstanceCard from '../containers/ConnectedCreateInstanceCard'

const containerStyle = {
  minHeight: '500px',
}

const Container = ({ instances, status }) =>
    <div style={containerStyle}>
        <Grid gutter={5}>
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
