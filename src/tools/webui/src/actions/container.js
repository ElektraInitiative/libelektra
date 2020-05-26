/**
 * @file
 *
 * @brief container (main overview) specific actions
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

export const ADD_INSTANCE = "ADD_INSTANCE";
export const UNADD_INSTANCE = "UNADD_INSTANCE";

export const addInstance = () => {
  return { type: ADD_INSTANCE };
};

export const unaddInstance = () => {
  return { type: UNADD_INSTANCE };
};
