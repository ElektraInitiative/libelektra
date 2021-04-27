/**
 * @file
 *
 * @brief small library to access Elektra’s kdb via node.js
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

const { exec } = require("child_process");
const { readFileSync, unlink } = require("fs");

const KDB_COMMAND = process.env.KDB || "kdb";

// constants
const ERR_KEY_NOT_FOUND = "Did not find key";
const ERROR_REGEX = /Sorry, module.*?([0-9]+):/;
const AT_REGEX = /At: (.*)$/;
const MOUNTPOINT_REGEX = /Mountpoint: (.*)$/;
const CONFIGFILE_REGEX = /Configfile: (.*)$/;

// KDBError
function KDBError(message) {
  this.name = "Elektra Error";
  let isError = false;
  this.details = message;
  let isNextMessageReason = false;
  for (let line of message.split("\n")) {
    let res;
    if (isNextMessageReason) {
      this.reason = this.reason + "\n" + line;
      isNextMessageReason = false;
    }
    if ((res = line.match(ERROR_REGEX))) {
      this.num = Number(res[1]);
      this.reason = line;
      isError = true;
      isNextMessageReason = true;
    } else if (isError) {
      if ((res = line.match(AT_REGEX))) {
        this.at = res[1];
      } else if ((res = line.match(MOUNTPOINT_REGEX))) {
        this.mountpoint = res[1];
      } else if ((res = line.match(CONFIGFILE_REGEX))) {
        this.configfile = res[1];
      }
    }
  }
  if (this.reason) {
    this.message = this.reason;
  } else {
    this.message = message || "";
  }
}
KDBError.prototype = Error.prototype;

// remove newline from the end of a string
const trimNewline = (str) => str.substring(0, str.length - 1);

// execute a script while catching and parsing errors
const safeExec = (script) =>
  new Promise((resolve, reject) =>
    exec(script, { maxBuffer: Infinity }, (err, stdout, stderr) => {
      if (err) {
        const errors = err.message.split("\n");
        // ignore error if it's "key not found"
        if (errors.length > 1 && errors[1].startsWith(ERR_KEY_NOT_FOUND)) {
          return resolve();
        } else {
          return reject(new KDBError(err.message));
        }
      }
      if (stderr) {
        if (stderr !== ERR_KEY_NOT_FOUND) {
          return reject(new KDBError(stderr));
        } else {
          return resolve(); // no key found, return no value
        }
      }
      const result = trimNewline(stdout);
      if (!result) {
        return resolve(""); // empty result, return empty string
      }
      return resolve(result);
    })
  );

// escape strings by surrounding them with ""
const escapeValues = (template, ...values) =>
  template.reduce((acc, part, i) => {
    /*
    Explanation of regular expression:
    - $1 `(\\\\)*` Matches an even number of _backslashes_
    - $4 Matches one _quote_ when there was an odd number of backslashes
    - $5 Matches one _quote_ when there was an even number of backslashes

    For instance in `\\\\\"`, $1 is `\\\\`, $4 is `"` and $5 is an empty string.
    So `\\\\\"` gets replaced to itself.
    In case of an even number of backslashes one backslash is added.

    (source: @krit0n - https://github.com/ElektraInitiative/libelektra/pull/983#discussion_r83965059)
    */
    let val = values[i - 1]
      .replace(/`/g, "\\`") // escape backticks
      .replace(/((\\\\)*)(\\(")|("))/g, "$1\\$4$5");
    if (typeof val === "string") val = `"${val}"`;
    return acc + val + part;
  });

const ELEKTRA_VERSION_REGEX = /KDB_VERSION\:\ ([0-9]+\.[0-9]+\.[0-9]+)\n/;

// get Elektra version
const version = () =>
  safeExec(`${KDB_COMMAND} --version`)
    .then((output) => {
      // parse result
      const matches = ELEKTRA_VERSION_REGEX.exec(output);
      if (!matches) {
        throw new Error("invalid version: " + output);
      }
      return matches;
    })
    .then((matches) => matches.length > 1 && matches[1]) // select version from matches
    .then((fullVersion) => {
      const splitVersions = fullVersion.split(".");
      return {
        version: fullVersion,
        major: Number(splitVersions[0]),
        minor: Number(splitVersions[1]),
        patch: Number(splitVersions[2]),
      };
    });

// list available paths under a given `path`
const ls = (path) =>
  safeExec(escapeValues`${KDB_COMMAND} ls -0 ${path}`).then(
    (stdout) => stdout && stdout.split("\0")
  );

// find paths given a search query
const find = (query) =>
  safeExec(escapeValues`${KDB_COMMAND} find -0 ${query}`)
    .then((stdout) => stdout && stdout.split("\0"))
    .then((res) => res || []);

// get value from given `path`
const get = (path) => safeExec(escapeValues`${KDB_COMMAND} get ${path}`);

// set value at given `path`
const set = (path, value) =>
  safeExec(escapeValues`${KDB_COMMAND} set -vd ${path} -- ${value}`);

// move value from given `path` to `destination`
const mv = (path, destination) =>
  safeExec(escapeValues`${KDB_COMMAND} mv -r ${path} ${destination}`);

// copy value from given `path` to `destination`
const cp = (path, destination) =>
  safeExec(escapeValues`${KDB_COMMAND} cp -r ${path} ${destination}`);

// remove single value at `path`
const rmSingle = (path) => safeExec(escapeValues`${KDB_COMMAND} rm ${path}`);

// remove value at given `path`
const rm = (path) => {
  return ls(path)
    .then((paths) =>
      Promise.all(
        paths.map((p) => {
          if (p.startsWith("user:/sw/elektra/web")) return { p, r: "1" }; // always restricted
          return getmeta(p, "restrict/remove")
            .then((r) => ({ p, r }))
            .catch((err) => ({ p, r: "0" })); // restrict/remove key not present
        })
      )
    )
    .then((restricted) =>
      Promise.all(
        restricted.map(({ p, r }) => {
          if (r !== "1") return rmSingle(p);
        })
      )
    );
};

// list meta values at given `path`
const lsmeta = (path) =>
  safeExec(escapeValues`${KDB_COMMAND} lsmeta -0 ${path}`).then(
    (stdout) => stdout && stdout.split("\0")
  );

// get meta value from given `path`
const getmeta = (path, meta) =>
  safeExec(escapeValues`${KDB_COMMAND} getmeta ${path} ${meta}`);

// set meta value at given `path`
const setmeta = (path, meta, value) =>
  safeExec(escapeValues`${KDB_COMMAND} setmeta ${path} ${meta} ${value}`);

// remove meta value at given `path`
const rmmeta = (path, meta) =>
  safeExec(escapeValues`${KDB_COMMAND} rmmeta ${path} ${meta}`);

// get all metavalues for given `path`
const getAllMeta = (path) =>
  lsmeta(path)
    .then(
      (metaValues) =>
        metaValues &&
        Promise.all(
          metaValues.map((meta) =>
            getmeta(path, meta).then((val) => {
              return { [meta]: val };
            })
          )
        )
    )
    // merge objects
    .then(
      (resolvedMetaValues) =>
        resolvedMetaValues && Object.assign.apply(Object, resolvedMetaValues)
    );

const getBufferFile = () => `/tmp/elektra-web.${Date.now()}.buffer.json`;

// export javascript object from given `path`
const _export = (path) => {
  const buffer = getBufferFile();
  return safeExec(
    escapeValues`${KDB_COMMAND} export ${path} yajl ${buffer}`
  ).then(() => {
    const data = JSON.parse(readFileSync(buffer));
    unlink(
      buffer,
      (err) => err && console.error("could not delete buffer file:", err)
    );
    return data;
  });
};

// import javascript object at given `path`
const _import = (path, value) =>
  safeExec(
    // we can trust JSON.stringify to escape values for us
    `echo '${JSON.stringify(value)}' | ` + // pipe json into kdb
      escapeValues`${KDB_COMMAND} import ${path} yajl`
  ).then((result) => _export(path));

// get value and available paths under a given `path`
const getAndLs = (path, { preload = 0 }) =>
  Promise.all(
    [ls(path), get(path), getAllMeta(path)] // execute ls and get in parallel
  ).then(([lsRes, value, meta]) => {
    let result = {
      exists: value !== undefined,
      name: path.split("/").pop(),
      path,
      ls: lsRes || [],
      value,
      meta,
    };
    if (preload > 0 && Array.isArray(lsRes)) {
      return Promise.all(
        lsRes
          .filter((p) => {
            const isNotSame = p !== path;
            const isNotDeeplyNested =
              p.split("/").length <= path.split("/").length + 1;
            return isNotSame && isNotDeeplyNested;
          })
          .map((p) => getAndLs(p, { preload: preload - 1 }))
      ).then((children) => {
        result.children = children;
        return result;
      });
    }
    return result; // return results as object
  });

// export kdb functions as `kdb` object
module.exports = {
  version,
  ls,
  get,
  getAndLs,
  set,
  mv,
  cp,
  rm,
  export: _export,
  import: _import,
  getmeta,
  setmeta,
  rmmeta,
  lsmeta,
  getAllMeta,
  find,
  KDB_COMMAND,
};
