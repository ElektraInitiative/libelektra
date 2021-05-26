/**
 * @file
 *
 * @brief Source for ElektraSettingsWriter
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "elektrasettingswriter.h"

int main (int argc, char ** argv)
{
  GApplication * app;
  int status;

  app = g_application_new ("org.libelektra.ElektraSettingsWriter", G_APPLICATION_IS_SERVICE);

  status = g_application_run (app, argc, argv);

  g_object_unref (app);

  return status;
}
