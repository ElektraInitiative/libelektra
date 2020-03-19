/**
 * @file
 *
 * @brief exports database operations regarding configuration options
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import kdb from "../kdb";
import { path } from "./utils";

export const getSingleInstanceOption = () => kdb.get(path("instance"));
