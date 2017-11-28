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

import React from 'react'

import { Card, CardHeader, CardText, CardActions } from 'material-ui/Card'
import FlatButton from 'material-ui/FlatButton'

import TreeView from './TreeView.jsx'
import CreateKey from './CreateKey.jsx'

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
const Configuration = ({
  instance, kdb, ls,
  returnToMain,
  getKey, setKey, deleteKey,
}) => {
  const { name, host } = instance

  const data = parseData(ls, kdb)

  return (
      <Card>
          <CardHeader
            title={`configuring instance "${name}"`}
            subtitle={host}
          />
          <CardText>
            <TreeView data={data} />
          </CardText>
          <CreateKey setKey={setKey} />
          <CardActions>
              <FlatButton
                label="done"
                onTouchTap={returnToMain}
              />
          </CardActions>
      </Card>
  )
}
export default Configuration
