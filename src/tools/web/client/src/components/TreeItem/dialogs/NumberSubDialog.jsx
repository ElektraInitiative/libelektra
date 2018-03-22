/**
 * @file
 *
 * @brief section of the settings dialog to modify number metadata of keys
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React, { Component } from 'react'

import TextField from 'material-ui/TextField'
import IconButton from 'material-ui/IconButton'
import FlatButton from 'material-ui/FlatButton'
import ActionDeleteIcon from 'material-ui/svg-icons/action/delete'
import ContentAddIcon from 'material-ui/svg-icons/content/add'

import SavedIcon from '../SavedIcon.jsx'
import debounce from '../../debounce'

const DebouncedTextField = debounce(TextField, { timeout: 250 })

class RangeItem extends Component {
  constructor (props, ...args) {
    super(props, ...args)
    const min = props.min || ''
    const max = props.max || ''
    this.state = {
      min,
      max,
      errorMin: min.trim().length <= 0,
      errorMax: max.trim().length <= 0,
    }
  }

  componentWillReceiveProps (nextProps) {
    const min = nextProps.min || ''
    const max = nextProps.max || ''
    this.setState({
      min,
      max,
      errorMin: min.trim().length <= 0,
      errorMax: max.trim().length <= 0,
    })
  }

  render () {
    const { onChange, onDelete } = this.props
    const { min, max, errorMin, errorMax } = this.state

    return (
      <span style={{ marginRight: 48 }}>
        <DebouncedTextField
          style={{ width: 35, marginRight: 4 }}
          floatingLabelText="min"
          floatingLabelFixed={true}
          value={min}
          underlineStyle={errorMin && { borderBottom: '2px solid rgb(244, 67, 54)' }}
          onChange={value => this.setState({ min: value })}
          onDebounced={value => {
            const { min, max } = this.state // pull updated values from state
            if (isNaN(min) || min.trim().length <= 0) {
              this.setState({ errorMin: true })
            } else {
              this.setState({ errorMin: false })
              if (!errorMax) onChange([ min, max ])
            }
          }}
        />
        {' — '}
        <DebouncedTextField
          style={{ width: 35, marginLeft: 8 }}
          floatingLabelText="max"
          floatingLabelFixed={true}
          value={max}
          underlineStyle={errorMax && { borderBottom: '2px solid rgb(244, 67, 54)' }}
          onChange={value => this.setState({ max: value })}
          onDebounced={value => {
            const { min, max } = this.state // pull updated values from state
            if (isNaN(max) || max.trim().length <= 0) {
              this.setState({ errorMax: true })
            } else {
              this.setState({ errorMax: false })
              if (!errorMin) onChange([ min, max ])
            }
          }}
        />
        <IconButton
          style={{ width: 22, height: 22, padding: 4 }}
          iconStyle={{ width: 14, height: 14 }}
          tooltip="delete range"
          onClick={onDelete}
        >
          <ActionDeleteIcon />
        </IconButton>
      </span>
    )
  }
}

class Ranges extends Component {
  constructor (props, ...args) {
    super(props, ...args)
    this.state = { ranges: this.parseRanges(props.ranges) || [['', '']] }
  }

  componentWillReceiveProps (nextProps) {
    this.setState({ ranges: this.parseRanges(nextProps.ranges) || [['', '']] })
  }

  parseRanges = (rangeStr) => {
    if (!rangeStr) return false
    try {
      return rangeStr.split(',').map(r => r.split('-'))
    } catch (err) {
      return false
    }
  }

  toRangeStr = (ranges) => {
    return ranges.map(r => r.join('-')).join(',')
  }

  render () {
    const { onChange } = this.props
    const { ranges } = this.state
    return (
        <div style={{ display: 'block' }}>
          {ranges.map(([ min, max ], i) =>
            <RangeItem
              key={'range' + i}
              min={min}
              max={max}
              onDelete={() => {
                const newState = ranges.filter((r, j) => i !== j)
                this.setState({ ranges: newState.length > 0 ? newState : [['', '']] })
                onChange(this.toRangeStr(newState))
              }}
              onChange={val => {
                const newState = ranges.map((r, j) => {
                  if (i === j) {
                    return val
                  }
                  return r
                })
                this.setState({ ranges: newState })
                onChange(this.toRangeStr(newState))
              }}
            />
          )}
          <FlatButton
            key="add"
            label="create new range"
            icon={<ContentAddIcon />}
            style={{ marginLeft: 16 }}
            onClick={() => {
              const newState = [ ...ranges, ['', ''] ]
              this.setState({ ranges: newState })
            }}
          />
        </div>
    )
  }
}

export default class NumberSubDialog extends Component {
  render () {
    const { value, saved, onChange } = this.props

    return (
        <div style={{ marginTop: 16 }}>
            <b>Ranges <SavedIcon saved={saved} /></b>
            <Ranges ranges={value} onChange={onChange} />
        </div>
    )
  }
}
