import React from 'react'

import { Card, CardHeader, CardText, CardActions } from 'material-ui/Card'
import FlatButton from 'material-ui/FlatButton'

import TreeItem from './TreeItem.jsx'
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

// render tree view from tree object
const createTreeView = ({ getKey, setKey, deleteKey, kdb }, tree, prefix = '') =>
  Object.keys(tree).map((key) => {
    const path = prefix ? `${prefix}/${key}` : key

    const loadChildren = () =>
      Object.keys(tree[key]).map(
        (childKey) => getKey(`${path}/${childKey}`)
      )

    // only allow deletion of items without subtrees
    const allowDelete = Object.keys(tree[key]).length === 0

    return (
        <TreeItem
          allowDelete={allowDelete}
          key={path}
          name={key + '/'}
          value={kdb && kdb[path]}
          onClick={loadChildren}
          onChange={(val) => setKey(path, val)}
          onDelete={() => deleteKey(path)}
        >
            {Object.keys(tree[key]).length > 0
              ? createTreeView({ getKey, setKey, deleteKey, kdb }, tree[key], path)
              : null
            }
        </TreeItem>
    )
  })

// get configuration of the selected cluster/instance
const getConfiguration = (instance, cluster) => {
  if (instance) {
    return {
      id: instance.id,
      name: instance.name,
      subtitle: instance.host,
    }
  } else if (cluster) {
    const instanceAmt = cluster.instances.length
    return {
      id: cluster.id,
      name: cluster.name,
      subtitle: `${instanceAmt} instance${instanceAmt !== 1 ? 's' : ''}`,
    }
  }
}

// configuration page
const Configuration = ({
  instance, cluster, configuring, kdb, ls,
  returnToMain,
  getKey, setKey, deleteKey,
  getClusterKey, setClusterKey, deleteClusterKey,
}) => {
  const { id, name, subtitle } = getConfiguration(instance, cluster)

  const getCorrectKey = (path) =>
    configuring === 'cluster'
      ? getClusterKey(id, path)
      : getKey(id, path)

  const setCorrectKey = (path, val) =>
    configuring === 'cluster'
      ? setClusterKey(id, path, val)
      : setKey(id, path, val)

  const deleteCorrectKey = (path) =>
    configuring === 'cluster'
      ? deleteClusterKey(id, path)
      : deleteKey(id, path)

  // add new keys from kdb to ls
  if (kdb) {
    Object.keys(kdb).map((path) => {
      if (ls.indexOf(path) === -1) ls.push(path)
    })
  }

  const tree = createTree(ls)
  const treeView = createTreeView({
    kdb,
    getKey: getCorrectKey,
    setKey: setCorrectKey,
    deleteKey: deleteCorrectKey,
  }, tree)

  return (
      <Card>
          <CardHeader
            title={`configuring ${configuring} "${name}"`}
            subtitle={subtitle}
          />
          <CardText>
            {treeView}
          </CardText>
          <CreateKey setKey={setCorrectKey} />
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
