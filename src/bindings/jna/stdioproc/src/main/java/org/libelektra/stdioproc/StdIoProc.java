package org.libelektra.stdioproc;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Objects;
import org.libelektra.KDBException;
import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;

class StdIoProc {

  private final Plugin plugin;
  private final Dump dump;

  final InputStream inputStream;
  final OutputStream outputStream;

  StdIoProc(Plugin plugin, InputStream inputStream, OutputStream outputStream) {
    this.plugin = plugin;
    this.inputStream = inputStream;
    this.outputStream = outputStream;
    this.dump = new Dump(this);
  }

  public boolean handshake() throws IOException {
    var header = readLine();
    if (!Objects.equals(header, "ELEKTRA_STDIOPROC INIT v1")) {
      return false;
    }

    println("ELEKTRA_STDIOPROC ACK v1");
    println(plugin.getName());
    // FIXME: plugin contract
    dump.write(
        KeySet.create(
            Key.create("system:/elektra/modules/stdioproc/exports/open", "1"),
            Key.create("system:/elektra/modules/stdioproc/exports/get", "1"),
            Key.create("system:/elektra/modules/stdioproc/exports/set", "1"),
            Key.create("system:/elektra/modules/stdioproc/exports/close", "1")));
    outputStream.flush();

    return true;
  }

  String readLine() throws IOException {
    // we can't use a BufferedReader, because we may need byte level access
    var sb = new StringBuilder();
    while (true) {
      var buffer = new byte[128];
      int b, i = 0;
      while ((b = inputStream.read()) >= 0 && i < 128) {
        if (b == '\n') {
          break;
        }
        buffer[i++] = (byte) b;
      }
      sb.append(new String(buffer, 0, i, StandardCharsets.UTF_8));
      if (b == '\n') {
        break;
      }
    }
    return sb.toString();
  }

  void println(String line) throws IOException {
    outputStream.write((line + "\n").getBytes(StandardCharsets.UTF_8));
    outputStream.flush();
  }

  void println() throws IOException {
    println("");
  }

  void printf(String format, Object... args) throws IOException {
    var s = String.format(format, args);
    outputStream.write(s.getBytes(StandardCharsets.UTF_8));
    outputStream.flush();
  }

  public boolean run() throws IOException {
    String line;
    while ((line = readLine()) != null) {
      switch (line) {
        case "open":
          {
            var parentKs = dump.read();
            if (parentKs == null) {
              System.exit(1);
              return false;
            }
            // FIXME: pass config to child
            var parent = parentKs.first();
            int result = plugin.open(KeySet.create(), parent);
            println(resultString(result));
            dump.write(KeySet.create(parent));
          }
          break;
        case "get":
          {
            var parentKs = dump.read();
            if (parentKs == null) {
              System.exit(1);
              return false;
            }
            var dataKs = dump.read();
            if (dataKs == null) {
              System.exit(1);
              return false;
            }
            var parent = parentKs.first();
            int result;
            try {
              result = plugin.get(dataKs, parent);
            } catch (KDBException e) {
              result = Plugin.STATUS_ERROR;
            }
            println(resultString(result));
            dump.write(KeySet.create(parent));
            dump.write(dataKs);
          }
          break;
        case "set":
          {
            var parentKs = dump.read();
            if (parentKs == null) {
              System.exit(1);
              return false;
            }
            var dataKs = dump.read();
            if (dataKs == null) {
              System.exit(1);
              return false;
            }
            var parent = parentKs.first();
            int result;
            try {
              result = plugin.set(dataKs, parent);
            } catch (KDBException e) {
              result = Plugin.STATUS_ERROR;
            }
            println(resultString(result));
            dump.write(KeySet.create(parent));
            dump.write(dataKs);
          }
          break;
        case "close":
          {
            var parentKs = dump.read();
            if (parentKs == null) {
              System.exit(1);
              return false;
            }
            var parent = parentKs.first();
            int result = plugin.close(parent);
            println(resultString(result));
            dump.write(KeySet.create(parent));
          }
          break;
        case "ELETKRA_STDIOPROC TERMINATE":
          return true;
      }
    }
    return false;
  }

  private static String resultString(int result) {
    switch (result) {
      case Plugin.STATUS_SUCCESS:
        return "success";
      case Plugin.STATUS_NO_UPDATE:
        return "noupdate";
      case Plugin.STATUS_ERROR:
      default:
        return "error";
    }
  }
}
