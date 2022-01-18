/**
 * @file
 *
 * @brief exports all available actions so they can be imported from ./actions
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

export * from "./instances";
export * from "./container";
export * from "./kdb";
export * from "./notification";

export const RESET_BATCH_UNDO = "RESET_BATCH_UNDO";

export const resetBatchUndo = () => {
  return { type: RESET_BATCH_UNDO };
};
