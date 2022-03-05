package org.libelektra.plugin;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Optional;
import javax.annotation.Nonnull;
import org.libelektra.ErrorCode;
import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;
import org.libelektra.ReadableKey;

/**
 * This class can be used to validate link via the "check/link" meta key. Initially it uses a {@link
 * URLConnection} to check if the link is reachable. You can supply an optional timeout for the
 * reachability check. If no timeout or an empty timeout is provided, the plugins defaults to
 * checking the validity of the URL via {@link URL} and its {@link MalformedURLException}.
 *
 * <p>Only checks the connection during `kdbSet()` operations.
 */
public class LinkPlugin implements Plugin {
  private static final String PLUGIN_NAME = "linkchecker";

  @Override
  public int open(KeySet conf, Key errorKey) {
    throw new UnsupportedOperationException();
  }

  @Override
  public int get(KeySet ks, Key parentKey) {
    if (parentKey.isBelowOrSame(Key.create(PROCESS_CONTRACT_ROOT))) {
      ks.append(
          Key.create(
              PROCESS_CONTRACT_ROOT + "/infos",
              "Link Checker Java plugin, loaded by the JNI plugin"));
      ks.append(Key.create(PROCESS_CONTRACT_ROOT + "/infos/provides", "check"));
      ks.append(Key.create(PROCESS_CONTRACT_ROOT + "/infos/placements", "presetstorage"));
      ks.append(Key.create(PROCESS_CONTRACT_ROOT + "/infos/author", "@aaronabebe"));
      ks.append(Key.create(PROCESS_CONTRACT_ROOT + "/infos/metadata", "check/link"));
      ks.append(
          Key.create(
              PROCESS_CONTRACT_ROOT + "/infos/description",
              "Check if a link is reachable or is a valid link"));
      ks.append(Key.create(PROCESS_CONTRACT_ROOT + "/infos/status", "preview"));
      ks.append(Key.create(PROCESS_CONTRACT_ROOT + "/exports/has/set", "1"));
      return STATUS_SUCCESS;
    }

    throw new UnsupportedOperationException();
  }

  @Override
  public int set(KeySet ks, Key parentKey) {
    boolean foundError = false;
    for (final Key key : ks) {
      Optional<ReadableKey> meta = key.getMeta("check/link");
      if (meta.isEmpty()) {
        continue;
      }

      String timeout = meta.get().getString();
      String keyValue = key.getString();

      if (timeout.isEmpty()) {
        if (!isValidURL(keyValue)) {
          parentKey.setError(
              ErrorCode.VALIDATION_SEMANTIC,
              "Found invalid link in key '" + key.getName() + "':" + keyValue);
          foundError = true;
        }
      } else {
        if (!isReachable(keyValue, Integer.parseInt(timeout))) {
          if (!isValidURL(keyValue)) {
            parentKey.setError(
                ErrorCode.VALIDATION_SEMANTIC,
                "Found unreachable link in key '" + key.getName() + "':" + keyValue);
            foundError = true;
          }
        }
      }
    }

    return foundError ? STATUS_ERROR : STATUS_SUCCESS;
  }

  @Override
  public int error(KeySet ks, Key parentKey) {
    throw new UnsupportedOperationException();
  }

  @Override
  public int close(Key parentKey) {
    throw new UnsupportedOperationException();
  }

  @Override
  @Nonnull
  public String getName() {
    return PLUGIN_NAME;
  }

  boolean isReachable(String link, int timeout) {
    try {
      final URL url = new URL(link);
      final URLConnection conn = url.openConnection();
      conn.setConnectTimeout(timeout);
      conn.connect();
      conn.getInputStream().close();
      return true;
    } catch (IOException e) {
      // URLConnection.connect() throws IOException if the host is not reachable
      return false;
    }
  }

  private boolean isValidURL(String link) {
    try {
      new URL(link);
      return true;
    } catch (MalformedURLException e) {
      // new URL() throws MalformedURLException if the URL is not valid
      return false;
    }
  }
}
