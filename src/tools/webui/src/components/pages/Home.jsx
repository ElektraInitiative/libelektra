/**
 * @file
 *
 * @brief this is the main overview of all instances
 *
 * it's a grid container showing instances. it also renders the
 * CreateInstanceCard when the user presses the "create instance" button.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import React from "react";

import ConnectedInstanceCard from "../../containers/ConnectedInstanceCard";
import ConnectedCreateInstanceCard from "../../containers/ConnectedCreateInstanceCard";

const containerStyle = {
  display: "flex",
  flexWrap: "wrap",
  minHeight: "500px",
};

const cellStyle = {
  flex: 1,
  maxWidth: "700px",
};

const Home = ({ instances, status }) => (
  <div style={containerStyle}>
    {instances &&
      instances.map((instance) => (
        <div key={instance.id} style={cellStyle}>
          <ConnectedInstanceCard
            id={instance.id}
            name={instance.name}
            host={instance.host}
            description={instance.description}
            visibility={instance.visibility}
          />
        </div>
      ))}
    {((status && status.addingInstance) ||
      !instances ||
      instances.length <= 0) && (
      <div key="addingInstance" style={cellStyle}>
        <ConnectedCreateInstanceCard />
      </div>
    )}
  </div>
);

export default Home;
