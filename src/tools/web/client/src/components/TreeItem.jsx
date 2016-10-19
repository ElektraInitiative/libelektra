import React from 'react'

import TextField from 'material-ui/TextField'

import TreeView from './TreeView.jsx'

// TODO: debounce onChange requests here, we're calling set every time it changes a little
export default class TreeItem extends React.Component {
  constructor (props) {
    super(props)
    this.state = { value: props.value }
  }

  render () {
    const { name, prefix, value, children, onClick, onChange } = this.props

    const val = this.state.value || value

    let valueField = val ? ( // if a key has a value, show a textfield
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
