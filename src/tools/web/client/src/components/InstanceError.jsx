/**
 * @file
 *
 * @brief shows instance (kdb) errors on the configuration page
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'
import FlatButton from 'material-ui/FlatButton'
import NavigationRefresh from 'material-ui/svg-icons/navigation/refresh'

const InstanceError = ({ instance, error, refresh }) => {
  const { host } = instance
  const { name, message } = error
  return (
    <div style={{ fontSize: '1.1em' }}>
      <h3 style={{ fontSize: '1.17em' }}>Connection to instance failed.</h3>
      <pre>
        <b>{name}:</b> {message}
      </pre>
      <p style={{ marginTop: '1.5em' }}>
        Please make sure <code>elektrad</code> is running on <code>{host}</code>
      </p>
      <p>
        Once it is running, you can
        <FlatButton
          label="refresh"
          icon={<NavigationRefresh />}
          onClick={refresh}
          style={{ marginLeft: '0.5em', marginRight: '0.5em' }}
        />
        to attempt reconnecting to the instance.
      </p>
    </div>
  )
}

export default InstanceError
