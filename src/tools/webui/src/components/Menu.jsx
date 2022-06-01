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

import React from "react";

import { Toolbar, ToolbarGroup, ToolbarTitle } from "material-ui/Toolbar";
import RaisedButton from "material-ui/RaisedButton";
import CircularProgress from "material-ui/CircularProgress";
import ContentAddIcon from "material-ui/svg-icons/content/add";
import ContentUndoIcon from "material-ui/svg-icons/content/undo";
import ContentRedoIcon from "material-ui/svg-icons/content/redo";
import NavigationArrowBack from "material-ui/svg-icons/navigation/arrow-back";
import NavigationChevronRight from "material-ui/svg-icons/navigation/chevron-right";
import { Link } from "react-router-dom";

const HEADER_MARGIN = "16px 10px 0 -10px";

const navigationArrowStyle = {
  margin: HEADER_MARGIN,
  color: "rgba(0, 0, 0, 0.4)",
  cursor: "pointer",
};

const breadcrumbStyle = {
  margin: "-25px 20px",
  color: "rgba(0, 0, 0, 0.4)",
  fontFamily: "Roboto",
};

// breadcrumb for menu title
const Breadcrumb = ({ name }) => (
  <div className="breadcrumb">
    <NavigationChevronRight
      style={{ color: "rgba(0, 0, 0, 0.4)", margin: HEADER_MARGIN }}
    />
    <div style={breadcrumbStyle}>{name}</div>
  </div>
);

// menu component
export default class Menu extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      name: "",
    };
  }

  resetValues() {
    this.setState({
      name: "",
    });
  }

  render() {
    const {
      loading,
      instances,
      subpage,
      status,
      singleInstanceMode,
      canUndo,
      canRedo,
      onUndo,
      onRedo,
      addInstance,
    } = this.props;

    const titleText = (
      <span className="titleText">
        elektra-web <small>v2.0</small>
      </span>
    );

    const title = (
      <ToolbarGroup>
        <div style={{ display: "flex" }}>
          {subpage &&
            !singleInstanceMode && ( // show back button on subpages
              <Link style={{ textDecoration: "none" }} to="/" tabIndex="0">
                <NavigationArrowBack style={navigationArrowStyle} />
              </Link>
            )}
          <Link style={{ textDecoration: "none" }} to="/" tabIndex="-1">
            <ToolbarTitle
              style={{
                fontFamily: "Roboto Light",
                fontSize: 22,
                letterSpacing: 0.79,
                color: "rgba(0,0,0,0.40)",
              }}
              text={titleText}
            />
          </Link>
          {subpage && (
            <Breadcrumb
              name={
                subpage // show breadcrumb on subpages
              }
            />
          )}
          {loading && ( // show spinner while waiting for responses
            <CircularProgress
              style={{ marginTop: 12, opacity: 0.7 }}
              size={32}
            />
          )}
        </div>
      </ToolbarGroup>
    );

    const mainActions = (
      <ToolbarGroup>
        <RaisedButton
          tabIndex={0}
          icon={<ContentAddIcon />}
          label="instance"
          primary={true}
          onClick={addInstance}
          disabled={
            (status && status.addingInstance) ||
            !instances ||
            instances.length <= 0
          }
        />
      </ToolbarGroup>
    );

    const subpageActions = (
      <ToolbarGroup>
        <RaisedButton
          tabIndex={0}
          icon={<ContentUndoIcon />}
          label="undo"
          onClick={onUndo}
          disabled={!canUndo}
        />
        <RaisedButton
          tabIndex={0}
          icon={<ContentRedoIcon />}
          label="redo"
          onClick={onRedo}
          disabled={!canRedo}
        />
      </ToolbarGroup>
    );

    return (
      <Toolbar>
        {title}
        {subpage ? subpageActions : mainActions}
      </Toolbar>
    );
  }
}
