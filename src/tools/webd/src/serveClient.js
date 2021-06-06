/**
 * @file
 *
 * @brief serve the client on the same port as the API
 *
 * in production mode, this serves static pre-built client files
 * in development mode, this serves the client directly via webpack, this allows
 * for hot reloading (automatically update components that got changed)
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import makeLog from "./log";
const { info } = makeLog("client");

import express from "express";
import { join as joinPath } from "path";

// adapted from https://github.com/rangle/serve-webpack-client
export default function createRouter({
  path,
  index = "index.html",
  dist = "build",
}) {
  const router = new express.Router();

  if (!path) {
    throw new Error("please specify a path");
  }

  const distPath = joinPath(path, dist);

  if (process.env.NODE_ENV === "production") {
    // In production, assets are bundled at build time and served statically
    // from the 'dist' folder. This is more efficient.
    info("prod mode: serving client static assets");
    router.use(express.static(distPath));
    router.get("*", (req, res) => res.sendFile(joinPath(distPath, index)));
  } else {
    // In development, assets are not served by the server
    info("dev mode: client assets not served by server");
  }

  return router;
}
