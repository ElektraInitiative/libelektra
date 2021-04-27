/**
 * @file
 *
 * @brief card used to create a new instance
 *
 * when the "create instance" button is pressed, this component is appended to
 * the main overview. it's like the instance card, but with editable fields that
 * are used to create the instance. once the instance is saved, it turns into a
 * normal InstanceCard
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from "react";

import { Card, CardHeader, CardText } from "material-ui/Card";
import FlatButton from "material-ui/FlatButton";
import TextField from "material-ui/TextField";
import SelectField from "material-ui/SelectField";
import MenuItem from "material-ui/MenuItem";

import { VISIBILITY_LEVELS, HOST_REGEX } from "../utils";

export default class CreateInstanceCard extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      name: "",
      host: "",
      description: "",
      visibility: "user",
      hostError: "",
    };
  }

  resetValues() {
    this.setState({
      name: "",
      host: "",
      description: "",
      visibility: "user",
      hostError: "",
    });
  }

  handleCreate = () => {
    const { createInstance, sendNotification } = this.props;
    const { name, host, description, visibility } = this.state;

    const nameEmpty = !name || name.trim().length <= 0;
    const hostEmpty = !host || host.trim().length <= 0;

    if (!nameEmpty && !hostEmpty) {
      createInstance({ name, host, description, visibility }).then(() =>
        sendNotification("Instance created successfully.")
      );
      this.resetValues();
    } else {
      alert("Please enter a name and host!");
    }
  };

  render() {
    const { instances, unaddInstance } = this.props;
    const { name, host, description, visibility, hostError } = this.state;

    const noInstancesYet = !instances || instances.length <= 0;
    const nameEmpty = !name || name.trim().length <= 0;
    const hostEmpty = !host || host.trim().length <= 0;

    return (
      <Card style={{ margin: "10px", marginBottom: "25px" }}>
        <CardHeader
          title={
            <span style={{ fontSize: 24, lineHeight: "30px" }}>
              {"add new instance"}
            </span>
          }
          subtitle="please enter a host and a friendly name"
        />
        <CardText>
          <div style={{ display: "flex" }}>
            <div style={{ flex: 1 }}>
              <TextField
                ref="nameField"
                floatingLabelText="name*"
                floatingLabelFixed={true}
                hintText="e.g. my webserver"
                onChange={(evt) => this.setState({ name: evt.target.value })}
                value={name}
              />
            </div>
            <div style={{ flex: 1 }}>
              <TextField
                ref="hostField"
                floatingLabelText="host*"
                floatingLabelFixed={true}
                errorText={hostError}
                onChange={(evt) => {
                  const newHost = evt.target.value;
                  this.setState({ host: newHost });
                  const [, matchedHost] = newHost.match(HOST_REGEX) || [];
                  if (!matchedHost) {
                    this.setState({
                      hostError: "invalid host, use http://host:port syntax",
                    });
                  } else {
                    this.setState({ hostError: "" });
                  }
                }}
                value={host}
                onKeyPress={(e) => {
                  if (e.key === "Enter") {
                    this.handleCreate();
                  }
                }}
              />
            </div>
          </div>
          <div style={{ display: "flex", marginTop: 4 }}>
            <div style={{ flex: 1 }}>
              <i>* required</i>
            </div>
            <div style={{ flex: 1 }}>
              <i>If elektrad is running on the same machine, host is: </i>
              <code>http://localhost:33333</code>
            </div>
          </div>
          <div style={{ display: "flex", marginTop: 16 }}>
            <div style={{ flex: 1 }}>
              <TextField
                ref="descriptionField"
                floatingLabelText="description"
                floatingLabelFixed={true}
                onChange={(evt) =>
                  this.setState({ description: evt.target.value })
                }
                value={description}
                onKeyPress={(e) => {
                  if (e.key === "Enter") {
                    this.handleCreate();
                  }
                }}
              />
            </div>
            <div style={{ flex: 1 }}>
              <SelectField
                floatingLabelText="visibility*"
                floatingLabelFixed={true}
                onChange={(e, _, val) => this.setState({ visibility: val })}
                value={visibility}
              >
                {Object.keys(VISIBILITY_LEVELS).map((lvl) => (
                  <MenuItem key={lvl} value={lvl} primaryText={">= " + lvl} />
                ))}
              </SelectField>
            </div>
          </div>
          <div style={{ marginTop: 32 }}>
            <FlatButton
              label="add"
              primary={true}
              onClick={this.handleCreate}
              disabled={nameEmpty || hostEmpty || hostError}
            />
            {!noInstancesYet && (
              <FlatButton
                label="cancel"
                onClick={() => {
                  unaddInstance();
                  this.resetValues();
                }}
              />
            )}
          </div>
        </CardText>
      </Card>
    );
  }
}
