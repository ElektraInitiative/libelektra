/**
 * @file
 *
 * @brief exports a function that creates logging functions
 *
 * e.g.
 * const { info, error } = makeLog('routes')
 * info('routes loaded!') // displays "webd:routes:info routes loaded!"
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import dude from "debug-dude";

import { name } from "../package.json";
export const namespace = name.split("/").pop();

export default function makeLog(name) {
  const appendName = name ? ":" + name : "";
  return dude(namespace + appendName);
}
