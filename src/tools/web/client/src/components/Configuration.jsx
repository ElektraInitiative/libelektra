import React from 'react'

import { Card, CardHeader, CardText, CardActions } from 'material-ui/Card'
import TextField from 'material-ui/TextField'
import FlatButton from 'material-ui/FlatButton'
import TreeView from './TreeView.jsx'

// TODO: debounce onChange requests here, we're calling set every time it changes a little
class TreeItem extends React.Component {
  constructor (props) {
    super(props)
    this.state = { value: props.value }
  }

  render () {
    const { name, prefix, value, children, onClick, onChange } = this.props

    const val = this.state.value || value

    let valueField = val ? (
        <span>
            {': '}
            <TextField
              id={`${prefix}_textfield`}
              value={val}
              floatingLabelText={this.state.saved ? 'saved!' : null}
              onChange={(evt) => {
                if (this.state.timeout) clearTimeout(this.state.timeout)
                const currentValue = evt.target.value
                this.setState({
                  value: evt.target.value,
                  timeout: setTimeout(() => {
                    onChange(currentValue)
                    this.setState({ saved: true })
                    setTimeout(() => {
                      this.setState({ saved: false })
                    }, 1000)
                  }, 500),
                })
              }}
            />
        </span>
    ) : null

    return (
        <TreeView
          nodeLabel={name}
          valueField={valueField}
          defaultCollapsed={true}
          onClick={onClick}
        >
            {children}
        </TreeView>
    )
  }
}

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

const createTreeView = ({ getKey, setKey, kdb }, tree, prefix = '') =>
  Object.keys(tree).map((key) => {
    const path = prefix ? `${prefix}/${key}` : key

    const loadChildren = () =>
      Object.keys(tree[key]).map(
        (childKey) => getKey(`${path}/${childKey}`)
      )

    return (
        <TreeItem
          key={path}
          name={key + '/'}
          value={kdb && kdb[path]}
          onClick={loadChildren}
          onChange={(val) => {console.log('set key to:', val);setKey(path, val)}}
        >
            {Object.keys(tree[key]).length > 0
              ? createTreeView({ getKey, setKey, kdb }, tree[key], path)
              : null
            }
        </TreeItem>
    )
  })

const Configuration = ({ instance, kdb, ls, returnToMain, getKey, setKey }) => {
  const { id, name, host } = instance
  const tree = createTree(ls)
  const treeView = createTreeView({
    kdb,
    getKey: (path) => getKey(id, path),
    setKey: (path, value) => setKey(id, path, value),
  }, tree)
  return (
      <Card>
          <CardHeader
            title={`configuring instance "${name}"`}
            subtitle={host}
          />
          <CardText>
            {treeView}
          </CardText>
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
