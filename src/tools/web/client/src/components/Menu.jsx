import React from 'react'

import { Toolbar, ToolbarGroup, ToolbarTitle } from 'material-ui/Toolbar'
import RaisedButton from 'material-ui/RaisedButton'
import TextField from 'material-ui/TextField'
import CircularProgress from 'material-ui/CircularProgress'
import ContentAddIcon from 'material-ui/svg-icons/content/add'
import NavigationArrowBack from 'material-ui/svg-icons/navigation/arrow-back'
import NavigationChevronRight from 'material-ui/svg-icons/navigation/chevron-right'

const HEADER_MARGIN = '16px 10px 0 -10px'

const navigationArrowStyle = {
  margin: HEADER_MARGIN,
  color: 'rgba(0, 0, 0, 0.4)',
  cursor: 'pointer',
}

const breadcrumbStyle = {
  margin: '-25px 20px',
  color: 'rgba(0, 0, 0, 0.4)',
  fontFamily: 'Roboto',
}

// breadcrumb for menu title
const Breadcrumb = ({ name }) =>
    <div className="breadcrumb">
        <NavigationChevronRight style={{color: 'rgba(0, 0, 0, 0.4)', margin: HEADER_MARGIN}} />
        <div style={breadcrumbStyle}>{name}</div>
    </div>

// menu component
export default class Menu extends React.Component {
  constructor (props) {
    super(props)
    this.state = {
      name: '',
    }
  }

  resetValues () {
    this.setState({
      name: '',
    })
  }

  render () {
    const { loading, addingCluster, subpage, clusterInstances } = this.props
    const { addInstance, addCluster, unaddCluster, returnToMain, createCluster } = this.props // action creators
    const title = (
        <ToolbarGroup>
            {subpage && // show back button on subpages
              <NavigationArrowBack style={navigationArrowStyle} onTouchTap={returnToMain} />}
            <ToolbarTitle
              style={{fontFamily: 'Roboto'}}
              text="elektra-web"
              onTouchTap={returnToMain}
            />
            {subpage && // show breadcrumb on subpages
              <Breadcrumb name={subpage} />}
            {loading && // show spinner while waiting for responses
              <CircularProgress style={{margin: '3px -15px'}} size={0.5} />}
        </ToolbarGroup>
    )

    const actions =
      addingCluster
      ? ( // modify toolbar in cluster creation mode (name field + create button)
          <ToolbarGroup>
              <TextField
                style={{marginTop: '5px', maxWidth: '150px'}}
                ref="nameField"
                hintText="cluster name"
                onChange={(evt) => this.setState({ name: evt.target.value })}
                value={this.state.name}
              />
              <RaisedButton
                style={{marginRight: '12px'}}
                label="create cluster"
                primary={true}
                onTouchTap={() => {
                  createCluster({
                    name: this.refs.nameField.getValue(),
                    instances: clusterInstances,
                  })
                  this.resetValues()
                }}
              />
              <RaisedButton
                style={{marginLeft: '12px'}}
                label="cancel"
                onTouchTap={() => {
                  unaddCluster()
                  this.resetValues()
                }}
              />
          </ToolbarGroup>
      )
      : ( // otherwise, show the default toolbar (add instance/cluster buttons)
          <ToolbarGroup>
              <RaisedButton
                icon={<ContentAddIcon />}
                label="instance"
                primary={true}
                onTouchTap={addInstance}
              />
              <RaisedButton
                icon={<ContentAddIcon />}
                label="cluster"
                primary={true}
                onTouchTap={addCluster}
              />
          </ToolbarGroup>
      )

    return (
        <Toolbar>
          {title}
          {actions}
        </Toolbar>
    )
  }
}
