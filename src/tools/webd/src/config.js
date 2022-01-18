/**
 * @file
 *
 * @brief configuration for webd, can be adjusted with environment variables
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import { getSingleInstanceOption } from "./db";

export const PORT = process.env.PORT || 33334;
export const ROOT_PATH =
  process.env.ROOT_PATH || "user:/sw/elektra/web/#0/current";

export const getSingleInstance = () => {
  // envvar overrides other configuration
  if (process.env.INSTANCE) return Promise.resolve(process.env.INSTANCE);

  return getSingleInstanceOption();
};
