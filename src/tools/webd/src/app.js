/**
 * @file
 *
 * @brief this is where the express app is created and modules/routes loaded
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

import express from "express";
import cookieSession from "cookie-session";
import bodyParser from "body-parser";
import serveClient from "./serveClient";
import path from "path";
import randomString from "crypto-random-string";

import initRoutes from "./routes";
import { PORT } from "./config";

export default function initApp(cb) {
  const app = express();

  app.use(
    cookieSession({
      name: "session",
      secret: randomString({ length: 16 }),
      maxAge: 24 * 60 * 60 * 1000, // 24 hours
    })
  );
  app.use(bodyParser.json());
  app.use(bodyParser.urlencoded({ extended: true }));
  app.use(bodyParser.text()); // parse raw text body, for kdb commands

  initRoutes(app);

  app.use(serveClient({ path: path.join(__dirname, "/../../webui") }));

  app.listen(PORT, () => cb(PORT)); // serve API at PORT
}
