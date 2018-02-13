/**
 * @file
 *
 * @brief this is the configuration page
 *
 * it renders the interactive tree view
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from 'react'

import { Card, CardHeader, CardText, CardActions } from 'material-ui/Card'
import FlatButton from 'material-ui/FlatButton'
import IconButton from 'material-ui/IconButton'
import NavigationRefresh from 'material-ui/svg-icons/navigation/refresh'
import { Link } from 'react-router-dom'

import TreeView from '../../containers/ConnectedTreeView'

// create tree structure from kdb ls result (list of available keys)
const partsTree = (acc, parts) => {
  if (parts.length <= 0) return acc

  const part = parts.shift()
  if (!acc[part]) acc[part] = {}
  partsTree(acc[part], parts)
  return acc
}

const createTree = (ls) =>
  ls.reduce((acc, item) => {
    return partsTree(acc, item.split('/'))
  }, {})

const parseDataSet = (getKey, instanceId, tree, path) => {
  return Object.keys(tree).map(key => {
    const newPath = path
      ? path + '/' + key
      : key
    const children = parseDataSet(getKey, instanceId, tree[key], newPath)
    return {
      name: key,
      path: newPath,
      children: (Array.isArray(children) && children.length > 0)
        ? () => {
          return new Promise(resolve => {
            children.map(child => getKey(instanceId, child.path))
            resolve(children)
          })
        } : false,
    }
  })
}

const parseData = (getKey, instanceId, ls, kdb) => {
  if (!Array.isArray(ls)) return
  const tree = createTree(ls)
  return parseDataSet(getKey, instanceId, tree)
}

// configuration page
export default class Configuration extends Component {
  constructor (props, ...rest) {
    super(props, ...rest)
    const { getKdb, match } = props
    const { id } = match && match.params
    getKdb(id)
  }

  refresh = () => {
    const { getKdb, match, sendNotification } = this.props
    const { id } = match && match.params
    sendNotification('refreshing configuration data...')
    getKdb(id)
      .then(() => {
        if (this.tree) return this.tree.refresh()
      })
      .then(() => sendNotification('configuration data refreshed!'))
  }

  render () {
    const {
      instance, ls, match, getKey,
    } = this.props

    if (!instance) {
      const title = (
          <h1><b>404</b> instance not found</h1>
      )
      return (
          <Card>
              <CardHeader title={title} />
          </Card>
      )
    }

    const { id } = match && match.params
    const { name, host } = instance
    const data = parseData(getKey, id, ls)

    const title = (
        <h1>
            <b>{name}</b>{' instance'}
            <IconButton
              className="refreshIcon"
              style={{ width: 28, height: 28 }}
              iconStyle={{ width: 16, height: 16 }}
              onClick={this.refresh}
              tooltip="refresh"
            >
                <NavigationRefresh />
            </IconButton>
        </h1>
    )

    return (
        <Card style={{ padding: '8px 16px' }}>
            <CardHeader title={title} subtitle={host} />
            <CardText>
                {data
                  ? <TreeView
                      treeRef={t => { this.tree = t }}
                      instanceId={id}
                      data={data}
                    />
                  : 'loading configuration data...'
                }
            </CardText>
            <CardActions>
                <Link to="/" style={{ textDecoration: 'none' }}>
                    <FlatButton primary label="done" />
                </Link>
            </CardActions>
        </Card>
    )
  }
}
