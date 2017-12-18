/**
 * @file
 *
 * @brief this is the configuration page
 *
 * it renders the interactive tree view and the CreateKey component to be able
 * to create new keys in the Elektra key database
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from 'react'

import { Card, CardHeader, CardText, CardActions } from 'material-ui/Card'
import FlatButton from 'material-ui/FlatButton'
import { Link } from 'react-router-dom'

import TreeView from '../TreeView.jsx'
import CreateKey from '../CreateKey.jsx'

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

const parseDataSet = (tree) =>
  Object.keys(tree).map(key => {
    return { name: key, children: parseDataSet(tree[key]) }
  })

const parseData = (ls, kdb) => {
  if (!Array.isArray(ls)) return

  // add new keys from kdb to ls
  if (kdb) {
    Object.keys(kdb).map((path) => {
      if (ls.indexOf(path) === -1) ls.push(path)
      return null
    })
  }
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
      instance, kdb, ls,
      getKey, setKey, deleteKey,
    } = this.props

    if (!instance) return (
        <Card>
            <CardHeader title="loading instance..." />
        </Card>
    )

    const { name, host } = instance

    const data = parseData(ls, kdb)

    return (
        <Card>
            <CardHeader
              title={`configuring instance "${name}"`}
              subtitle={host}
            />
            <CardText>
                {data
                  ? <TreeView data={data} />
                  : 'loading configuration data...'
                }
            </CardText>
            <CreateKey setKey={setKey} />
            <CardActions>
                <Link to="/" style={{ textDecoration: 'none' }}>
                    <FlatButton label="done" />
                </Link>
            </CardActions>
        </Card>
    )
  }
}
