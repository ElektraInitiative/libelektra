/**
 * @file
 *
 * @brief instance specific actions
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { thunkCreator, parseJSONResponse } from "./utils";
import { HOST_REGEX } from "../utils";

const cleanInstanceData = (data) => {
  if (data && data.host) {
    const [, host] = data.host.match(HOST_REGEX);
    if (host) {
      return { ...data, host };
    }
  }
  return data;
};

// ~~~

export const INSTANCES_REQUEST = "INSTANCES_REQUEST";
export const INSTANCES_SUCCESS = "INSTANCES_SUCCESS";
export const INSTANCES_FAILURE = "INSTANCES_FAILURE";

export const fetchInstances = () =>
  thunkCreator({
    types: [INSTANCES_REQUEST, INSTANCES_SUCCESS, INSTANCES_FAILURE],
    promise: fetch(`/api/instances`, { credentials: "same-origin" }).then(
      parseJSONResponse
    ),
  });

// ~~~

export const INSTANCE_REQUEST = "INSTANCE_REQUEST";
export const INSTANCE_SUCCESS = "INSTANCE_SUCCESS";
export const INSTANCE_FAILURE = "INSTANCE_FAILURE";

export const fetchInstance = (id) =>
  thunkCreator({
    types: [INSTANCE_REQUEST, INSTANCE_SUCCESS, INSTANCE_FAILURE],
    promise: fetch(`/api/instances/${id}`, { credentials: "same-origin" }).then(
      parseJSONResponse
    ),
  });

// ~~~

export const INSTANCE_UPDATE_REQUEST = "INSTANCE_UPDATE_REQUEST";
export const INSTANCE_UPDATE_SUCCESS = "INSTANCE_UPDATE_SUCCESS";
export const INSTANCE_UPDATE_FAILURE = "INSTANCE_UPDATE_FAILURE";

export const updateInstance = (id, data) =>
  thunkCreator({
    types: [
      INSTANCE_UPDATE_REQUEST,
      INSTANCE_UPDATE_SUCCESS,
      INSTANCE_UPDATE_FAILURE,
    ],
    promise: fetch(`/api/instances/${id}`, {
      credentials: "same-origin",
      method: "PUT",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(cleanInstanceData(data)),
    }).then(parseJSONResponse),
  });

// ~~~

export const INSTANCE_DELETE_REQUEST = "INSTANCE_DELETE_REQUEST";
export const INSTANCE_DELETE_SUCCESS = "INSTANCE_DELETE_SUCCESS";
export const INSTANCE_DELETE_FAILURE = "INSTANCE_DELETE_FAILURE";

export const deleteInstance = (id, data) =>
  thunkCreator({
    types: [
      INSTANCE_DELETE_REQUEST,
      INSTANCE_DELETE_SUCCESS,
      INSTANCE_DELETE_FAILURE,
    ],
    promise: fetch(`/api/instances/${id}`, {
      credentials: "same-origin",
      method: "DELETE",
    }).then(parseJSONResponse),
  });

// ~~~

export const CREATE_INSTANCE_REQUEST = "CREATE_INSTANCE_REQUEST";
export const CREATE_INSTANCE_SUCCESS = "CREATE_INSTANCE_SUCCESS";
export const CREATE_INSTANCE_FAILURE = "CREATE_INSTANCE_FAILURE";

export const createInstance = (data) =>
  thunkCreator({
    types: [
      CREATE_INSTANCE_REQUEST,
      CREATE_INSTANCE_SUCCESS,
      CREATE_INSTANCE_FAILURE,
    ],
    promise: fetch(`/api/instances`, {
      credentials: "same-origin",
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(cleanInstanceData(data)),
    }).then(parseJSONResponse),
  });
