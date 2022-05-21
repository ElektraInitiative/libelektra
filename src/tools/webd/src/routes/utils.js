/**
 * @file
 *
 * @brief utility functions used in routes
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { ROOT_PATH } from "../config";

export function APIError(message) {
  this.name = "APIError";
  this.message = message || "";
}
APIError.prototype = Error.prototype;

export const prettyprint = (obj) => JSON.stringify(obj, null, 2);

export const successResponse = (res, output) =>
  output ? res.json(output) : res.send();

export const errorResponse = (res, err) => {
  if (process.env.NODE_ENV !== "production") {
    if (!(err instanceof APIError)) console.error(err);
  }
  const errObj =
    err instanceof Error ? { name: err.name, message: err.message } : err;
  return res.status(400).type("application/json").json({ error: errObj });
};

const sessionRegex = /session=([a-zA-Z0-9-]*)/;

export const getSessionID = (instanceId, cookie) => {
  const sessionId =
    cookie.sessions && cookie.sessions[instanceId]
      ? cookie.sessions[instanceId]
      : "";

  return sessionId;
};

export const setSessionID = (instanceId, cookie, response) => {
  if (!response.headers) {
    return response;
  }

  const setCookie = response.headers.get("Set-Cookie");

  if (setCookie) {
    if (!cookie.sessions) {
      cookie.sessions = {};
    }

    const sessionId = setCookie.match(sessionRegex)[0];

    cookie.sessions[instanceId] = sessionId;
  }

  return response;
};

// don't show the internal database via the API
export const dontShowDB = (output) => {
  if (output && Array.isArray(output.ls)) {
    output.ls = output.ls.filter((path) => !path.startsWith(ROOT_PATH));
  }
  return output;
};
