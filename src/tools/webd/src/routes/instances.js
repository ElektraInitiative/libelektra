/**
 * @file
 *
 * @brief this exports a function that defines routes for the express app
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import {
  APIError,
  successResponse,
  errorResponse,
  dontShowDB,
  setSessionID,
  getSessionID,
} from "./utils";

import {
  getInstances as getDBInstances,
  createInstance,
  getInstance as getDBInstance,
  updateInstance,
  deleteInstance,
} from "../db";

import { getSingleInstance } from "../config";

import remoteKdb from "../connector";

// match VISIBILITY@HOST, while ignoring tailing slashes
const SINGLE_INSTANCE_REGEX = /^((.*)\@)?(https?\:\/\/[^\/]*)(\/.*)?$/;

const makeMyInstance = (str) => {
  if (!str) return false;
  const [, , visibility, host] = str.match(SINGLE_INSTANCE_REGEX);
  if (!host) {
    console.error(
      `Invalid single instance string \'${str}\'. Format should be: VISIBILITY@HOST, e.g. user@http://localhost:33333`
    );
    return false;
  }
  return {
    host,
    id: "my",
    name: "My",
    description: "Single instance mode",
    visibility: visibility || "user",
  };
};

const getInstance = (id) =>
  id === "my" ? getSingleInstance().then(makeMyInstance) : getDBInstance(id);

const getInstances = () =>
  getSingleInstance().then((host) => {
    if (!host) return getDBInstances();
    return getDBInstances().then((instances) =>
      instances.concat([makeMyInstance(host)])
    );
  });

export default function initInstanceRoutes(app) {
  app
    .route("/api/instances")
    .get((req, res) =>
      getInstances()
        .then((output) => successResponse(res, output))
        .catch((err) => errorResponse(res, err))
    )
    .post((req, res) =>
      createInstance(req.body)
        .then((output) => successResponse(res.status(201), output))
        .catch((err) => errorResponse(res, err))
    );

  app
    .route("/api/instances/:id")
    .get((req, res) =>
      getInstance(req.params.id)
        .then((output) => successResponse(res, output))
        .catch((err) => errorResponse(res, err))
    )
    .put((req, res) =>
      updateInstance(req.params.id, req.body)
        .then((output) => successResponse(res, output))
        .catch((err) => errorResponse(res, err))
    )
    .delete((req, res) =>
      deleteInstance(req.params.id)
        .then((output) => successResponse(res, output))
        .catch((err) => errorResponse(res, err))
    );

  app.get("/api/instances/:id/version", (req, res) =>
    getInstance(req.params.id)
      .then((instance) => {
        if (!instance || !instance.host) {
          throw new APIError("Instance not found or invalid (no host)");
        }
        return remoteKdb.version(instance.host);
      })
      .then((instanceRes) =>
        setSessionID(req.params.id, req.session, instanceRes)
      )
      .then((output) => successResponse(res, output))
      .catch((err) => errorResponse(res, err))
  );

  app.get("/api/instances/:id/kdb", (req, res) =>
    getInstance(req.params.id)
      .then((instance) => {
        if (!instance || !instance.host) {
          throw new APIError("Instance not found or invalid (no host)");
        }
        return remoteKdb.get(instance.host, {
          sessionId: getSessionID(instance.id, req.session),
        });
      })
      .then((instanceRes) =>
        setSessionID(req.params.id, req.session, instanceRes)
      )
      .then((res) => res.json())
      .then(dontShowDB)
      .then((output) => successResponse(res, output))
      .catch((err) => errorResponse(res, err))
  );

  app
    .route("/api/instances/:id/kdb/*")
    .get((req, res) =>
      getInstance(req.params.id)
        .then((instance) => {
          const qs = req._parsedUrl.query;
          return remoteKdb.get(instance.host, {
            query: qs ? "?" + qs : "",
            sessionId: getSessionID(instance.id, req.session),
            path: req.params[0],
          });
        })
        .then((instanceRes) =>
          setSessionID(req.params.id, req.session, instanceRes)
        )
        .then((res) => res.json())
        .then(dontShowDB)
        .then((output) => successResponse(res, output))
        .catch((err) => errorResponse(res, err))
    )
    .put((req, res) =>
      getInstance(req.params.id)
        .then((instance) =>
          remoteKdb.set(instance.host, req.params[0], req.body)
        )
        .then((instanceRes) =>
          setSessionID(req.params.id, req.session, instanceRes)
        )
        .then((output) => successResponse(res, output))
        .catch((err) => errorResponse(res, err))
    )
    .delete((req, res) =>
      getInstance(req.params.id)
        .then((instance) => remoteKdb.rm(instance.host, req.params[0]))
        .then((instanceRes) =>
          setSessionID(req.params.id, req.session, instanceRes)
        )
        .then((output) => successResponse(res, output))
        .catch((err) => errorResponse(res, err))
    );

  app.get("/api/instances/:id/kdbFind/*", (req, res) =>
    getInstance(req.params.id)
      .then((instance) => remoteKdb.find(instance.host, req.params[0]))
      .then((instanceRes) =>
        setSessionID(req.params.id, req.session, instanceRes)
      )
      .then((output) => successResponse(res, output))
      .catch((err) => errorResponse(res, err))
  );

  app.post("/api/instances/:id/kdbMv/*", (req, res) =>
    getInstance(req.params.id)
      .then((instance) => remoteKdb.mv(instance.host, req.params[0], req.body))
      .then((instanceRes) =>
        setSessionID(req.params.id, req.session, instanceRes)
      )
      .then(() => res.status(204).send())
      .catch((err) => errorResponse(res, err))
  );

  app.post("/api/instances/:id/kdbCp/*", (req, res) =>
    getInstance(req.params.id)
      .then((instance) => remoteKdb.cp(instance.host, req.params[0], req.body))
      .then((instanceRes) =>
        setSessionID(req.params.id, req.session, instanceRes)
      )
      .then(() => res.status(204).send())
      .catch((err) => errorResponse(res, err))
  );

  app
    .route("/api/instances/:id/kdbMeta/*")
    .post((req, res) =>
      getInstance(req.params.id)
        .then((instance) =>
          remoteKdb.setmeta(
            instance.host,
            req.params[0],
            req.body.key,
            req.body.value
          )
        )
        .then((instanceRes) =>
          setSessionID(req.params.id, req.session, instanceRes)
        )
        .then(() => res.status(204).send())
        .catch((err) => errorResponse(res, err))
    )
    .delete((req, res) =>
      getInstance(req.params.id)
        .then((instance) =>
          remoteKdb.rmmeta(instance.host, req.params[0], req.body.key)
        )
        .then((instanceRes) =>
          setSessionID(req.params.id, req.session, instanceRes)
        )
        .then(() => res.status(204).send())
        .catch((err) => errorResponse(res, err))
    );

  app
      .route("/api/instances/:id/kdbMetaBulk/*")
      .post((req, res) =>
          getInstance(req.params.id)
              .then((instance) =>
                  remoteKdb.setmetabulk(
                      instance.host,
                      req.params[0],
                      req.body
                  )
              )
              .then((instanceRes) =>
                  setSessionID(req.params.id, req.session, instanceRes)
              )
              .then(() => res.status(204).send())
              .catch((err) => errorResponse(res, err))
      );
}
