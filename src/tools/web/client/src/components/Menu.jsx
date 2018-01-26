/**
 * @file
 *
 * @brief this is the menu (top bar) of the application
 *
 * it shows the current page and, if on the overview page, it will show a button
 * to create instances
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from 'react'

import { Toolbar, ToolbarGroup, ToolbarTitle } from 'material-ui/Toolbar'
import RaisedButton from 'material-ui/RaisedButton'
import CircularProgress from 'material-ui/CircularProgress'
import ContentAddIcon from 'material-ui/svg-icons/content/add'
import NavigationArrowBack from 'material-ui/svg-icons/navigation/arrow-back'
import NavigationChevronRight from 'material-ui/svg-icons/navigation/chevron-right'
import { Link } from 'react-router-dom'

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
    const { loading, subpage, status } = this.props
    const { addInstance } = this.props // action creators
    const title = (
        <ToolbarGroup>
          <div style={{ display: 'flex' }}>
            {subpage && // show back button on subpages
              <Link style={{ textDecoration: 'none' }} to="/"><NavigationArrowBack style={navigationArrowStyle} /></Link>}
            <Link style={{ textDecoration: 'none' }} to="/">
              <ToolbarTitle
                style={{ fontFamily: 'Roboto Light', fontSize: 22, letterSpacing: 0.79, color: 'rgba(0,0,0,0.40)' }}
                text="elektra-web"
              />
            </Link>
            {subpage && // show breadcrumb on subpages
              <Breadcrumb name={subpage} />}
            {loading && // show spinner while waiting for responses
              <CircularProgress style={{ marginTop: 12, opacity: 0.7 }} size={32} />}
          </div>
        </ToolbarGroup>
    )

    const actions = (
        <ToolbarGroup>
            <RaisedButton
              icon={<ContentAddIcon />}
              label="instance"
              primary={true}
              onTouchTap={addInstance}
              disabled={status && status.addingInstance}
            />
        </ToolbarGroup>
    )

    return (
        <Toolbar>
          {title}
          {!subpage && actions /* don't show action buttons on subpages */}
        </Toolbar>
    )
  }
}
