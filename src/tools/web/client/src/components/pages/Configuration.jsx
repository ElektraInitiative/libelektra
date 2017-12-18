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

const parseDataSet = (tree, path) =>
  Object.keys(tree).map(key => {
    const newPath = path
      ? path + '/' + key
      : key
    const children = parseDataSet(tree[key], newPath)
    return {
      name: key,
      path: newPath,
      children: (Array.isArray(children) && children.length > 0)
        ? children
        : false,
    }
  })

const parseData = (ls, kdb) => {
  if (!Array.isArray(ls)) return
  const tree = createTree(ls)
  return parseDataSet(tree)
}

// configuration page
export default class Configuration extends Component {
  constructor (props, ...rest) {
    super(props, ...rest)
    const { getKdb, match } = props
    const { id } = match && match.params
    getKdb(id)
  }

  render () {
    const {
      instance, ls, match,
    } = this.props

    if (!instance) return (
        <Card>
            <CardHeader title="loading instance..." />
        </Card>
    )

    const { id } = match && match.params
    const { name, host } = instance
    const data = parseData(ls)

    const title = (
        <span style={{ fontSize: 24, lineHeight: '50px' }}>
            <b>{name}</b>
            <span style={{ fontFamily: 'Roboto Light' }}>{' instance'}</span>
        </span>
    )

    return (
        <Card style={{ padding: '8px 16px' }}>
            <CardHeader title={title} subtitle={host} />
            <CardText>
                {data
                  ? <TreeView instanceId={id} data={data} />
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
