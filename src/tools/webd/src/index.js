/**
 * @file
 *
 * @brief the main entry point, this file gets executed when webd is started
 *
 * it will detect if Elektra (with the yajl plugin) is installed and show an
 * error if that isn't the case. otherwise, it will show version information
 * from Elektra and initialize webd
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import makeLog from "./log";
const { info, error } = makeLog();

import {
  name as packageName,
  version as packageVersion,
} from "../package.json";
import getVersions from "./versions";
import initApp from "./app";
import kdb from "./kdb";

import { getInstances } from "./db";

info(`%s v%s starting`, packageName, packageVersion);
getVersions()
  .then((versions) => {
    if (!versions.elektra) {
      error(`couldn't detect elektra version`);
      error(`are you sure you have libelektra and kdb installed?`);
      process.exit(1);
    } else {
      const { major, minor, patch } = versions.elektra;
      const versionSupported = major >= 0 && minor >= 9 && patch >= 0;
      if (!versionSupported) {
        error(
          `you are running an old libelektra version ${major}.${minor}.${patch}, which is not supported`
        );
        error(`please upgrade to libelektra 0.9.0 or higher`);
        process.exit(1);
      }
      return getInstances() // make sure yajl is installed
        .then(() => {
          if (kdb.KDB_COMMAND === "kdb") info(`|- using default kdb command`);
          else info(`|- using kdb from: ${kdb.KDB_COMMAND}`);
          info(`|- versions: %o`, versions);
          initApp((port) => info(`\`-> running on http://localhost:${port}`));
        })
        .catch((err) => {
          if (err.message.includes("Was not able to load such a plugin!")) {
            error(`missing dependencies`);
            error(`the yajl plugin is not installed for libelektra`);
            process.exit(1);
          } else throw err; // re-throw error
        });
    }
  })
  .catch((err) => error(`error while starting %s: %o`, packageName, err));
