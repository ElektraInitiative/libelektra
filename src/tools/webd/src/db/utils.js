/**
 * @file
 *
 * @brief utility functions used in the database layer
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

export { default as generateId } from "uuid/v4";

import { ROOT_PATH } from "../config";

export const path = (path) => ROOT_PATH + "/" + path;

export const virtualKdb = (clusterId, kdbPath) =>
  path(`vkdb/${clusterId}${kdbPath ? "/" + kdbPath : ""}`);

export const findById = (id) => (elements) =>
  elements.find((element) => element.id === id);

export const updateById = (id, update) => (elements) =>
  elements.reduce((acc, element) => {
    if (element.id === id) {
      acc.push(update(element));
    } else {
      acc.push(element);
    }
    return acc;
  }, []);

export const deleteById = (id) => (elements) =>
  elements.filter((element) => element.id !== id);
