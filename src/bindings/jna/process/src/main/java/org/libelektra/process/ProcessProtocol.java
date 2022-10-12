package org.libelektra.process;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.util.Objects;
import org.libelektra.KDBException;
import org.libelektra.Key;
import org.libelektra.KeySet;
import org.libelektra.Plugin;

class ProcessProtocol {

  private static final String STATUS_SUCCESS = "success";
  private static final String STATUS_NO_UPDATE = "noupdate";
  private static final String STATUS_ERROR = "error";

  private final Plugin plugin;
  private final Dump dump;

  final InputStream inputStream;
  final OutputStream outputStream;

  ProcessProtocol(Plugin plugin, InputStream inputStream, OutputStream outputStream) {
    this.plugin = plugin;
    this.inputStream = inputStream;
    this.outputStream = outputStream;
    this.dump = new Dump(this);
  }

  public boolean handshake() throws IOException {
    var header = readLine();
    if (!Objects.equals(header, "ELEKTRA_PROCESS INIT v1")) {
      return false;
    }

    println("ELEKTRA_PROCESS ACK v1");
    println(plugin.getName());

    var jniContract = KeySet.create();
    try {
      plugin.get(jniContract, Key.create(Plugin.PROCESS_CONTRACT_ROOT));
    } catch (KDBException e) {
      // ignored
    }

    var contract = KeySet.create();
    for (Key key : jniContract) {
      Key k = key.dup();
      k.setName(
          key.getName().replace(Plugin.PROCESS_CONTRACT_ROOT, "system:/elektra/modules/process"));
      contract.append(k);
    }

    dump.write(contract);

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
              return false;
            }
            var config = dump.read();
            if (config == null) {
              return false;
            }
            var parent = parentKs.first();
            executeOperation(parent, pk -> plugin.open(config, pk));
          }
          break;
        case "get":
          {
            var parentKs = dump.read();
            if (parentKs == null) {
              return false;
            }
            var dataKs = dump.read();
            if (dataKs == null) {
              return false;
            }
            var parent = parentKs.first();
            executeOperation(parent, pk -> plugin.get(dataKs, pk));
            dump.write(dataKs);
          }
          break;
        case "set":
          {
            var parentKs = dump.read();
            if (parentKs == null) {
              return false;
            }
            var dataKs = dump.read();
            if (dataKs == null) {
              return false;
            }
            var parent = parentKs.first();
            executeOperation(parent, pk -> plugin.set(dataKs, pk));
            dump.write(dataKs);
          }
          break;
        case "close":
          {
            var parentKs = dump.read();
            if (parentKs == null) {
              return false;
            }
            var parent = parentKs.first();
            executeOperation(parent, plugin::close);
          }
          break;
        case "ELETKRA_PROCESS TERMINATE":
          return true;
      }
    }
    return false;
  }

  private void executeOperation(Key parent, OperationCallback callback) throws IOException {
    String result;
    try {
      switch (callback.call(parent)) {
        case Plugin.STATUS_SUCCESS:
          result = STATUS_SUCCESS;
          break;
        case Plugin.STATUS_NO_UPDATE:
          result = STATUS_NO_UPDATE;
          break;
        case Plugin.STATUS_ERROR:
        default:
          result = STATUS_ERROR;
          break;
      }
    } catch (KDBException e) {
      result = STATUS_ERROR;
    }
    println(result);
    dump.write(KeySet.create(parent));
  }

  interface OperationCallback {
    int call(Key parentKey) throws KDBException;
  }
}
