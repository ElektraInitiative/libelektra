package org.libelektra.stdioproc;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Scanner;
import javax.annotation.Nullable;
import org.libelektra.Key;
import org.libelektra.KeyPointer;
import org.libelektra.KeySet;
import org.libelektra.ReadableKey;

class Dump {
  private final StdIoProc stdIoProc;

  public Dump(StdIoProc stdIoProc) {

    this.stdIoProc = stdIoProc;
  }

  @Nullable
  public KeySet read() throws IOException {
    var header = stdIoProc.readLine();
    if (!Objects.equals(header, "kdbOpen 2")) {
      return null;
    }

    KeySet ks = KeySet.create();

    String line;
    Key key = null;
    while ((line = stdIoProc.readLine()) != null) {
      var scanner = new Scanner(line);
      scanner.useDelimiter(" ");
      var cmd = scanner.next();
      switch (cmd) {
        case "$key":
          {
            var type = scanner.next();
            var nameSize = scanner.nextInt();
            var valueSize = scanner.nextInt();

            var buffer = stdIoProc.inputStream.readNBytes(nameSize);
            if (buffer == null || buffer.length != nameSize) {
              return null;
            }
            if (stdIoProc.inputStream.read() != '\n') {
              return null;
            }
            var name = new String(buffer, StandardCharsets.UTF_8);

            buffer = stdIoProc.inputStream.readNBytes(valueSize);
            if (buffer == null || buffer.length != valueSize) {
              return null;
            }
            if (stdIoProc.inputStream.read() != '\n') {
              return null;
            }

            key = Key.create(name);
            if (Objects.equals(type, "string")) {
              var value = new String(buffer);
              key.setString(value);
            } else {
              key.setBinary(buffer);
            }
            ks.append(key);
          }
          break;
        case "$meta":
          {
            var nameSize = scanner.nextInt();
            var valueSize = scanner.nextInt();

            var buffer = stdIoProc.inputStream.readNBytes(nameSize);
            if (buffer == null || buffer.length != nameSize) {
              return null;
            }
            if (stdIoProc.inputStream.read() != '\n') {
              return null;
            }
            var name = new String(buffer, StandardCharsets.UTF_8);

            buffer = stdIoProc.inputStream.readNBytes(valueSize);
            if (buffer == null || buffer.length != valueSize) {
              return null;
            }
            if (stdIoProc.inputStream.read() != '\n') {
              return null;
            }
            var value = new String(buffer, StandardCharsets.UTF_8);

            if (key == null) {
              return null;
            }
            key.setMeta(name, value);
          }
          break;
        case "$copymeta":
          {
            var keyNameSize = scanner.nextInt();
            var metaNameSize = scanner.nextInt();

            var buffer = stdIoProc.inputStream.readNBytes(keyNameSize);
            if (buffer == null || buffer.length != keyNameSize) {
              return null;
            }
            if (stdIoProc.inputStream.read() != '\n') {
              return null;
            }
            var keyName = new String(buffer, StandardCharsets.UTF_8);

            buffer = stdIoProc.inputStream.readNBytes(metaNameSize);
            if (buffer == null || buffer.length != metaNameSize) {
              return null;
            }
            if (stdIoProc.inputStream.read() != '\n') {
              return null;
            }
            var metaName = new String(buffer, StandardCharsets.UTF_8);

            if (key == null) {
              return null;
            }

            Optional<Key> source = ks.lookup(keyName);
            if (source.isEmpty()) {
              return null;
            }

            key.copyMeta(source.get(), metaName);
          }
          break;
        case "$end":
          return ks;
        default:
          return null;
      }
    }
    return null;
  }

  public void write(KeySet keySet) throws IOException {
    stdIoProc.println("kdbOpen 2");
    Map<KeyPointer, String> metaCopies = new HashMap<>();
    for (Key key : keySet) {
      var nameSize = Math.max(0, key.getNameSize() - 1);
      stdIoProc.printf(
          "$key %s %d %d\n",
          key.isBinary() ? "binary" : "string", nameSize, Math.max(0, key.getValueSize() - 1));
      stdIoProc.println(key.getName());
      if (key.isBinary()) {
        stdIoProc.outputStream.flush();
        stdIoProc.outputStream.write(key.getBinary());
        stdIoProc.println();
      } else {
        stdIoProc.println(key.getString());
      }

      key.rewindMeta();
      while (key.nextMeta().isPresent()) {
        ReadableKey meta = key.currentMeta();
        KeyPointer keyPointer = new KeyPointer(meta);
        if (metaCopies.containsKey(keyPointer)) {
          stdIoProc.println(metaCopies.get(keyPointer));
        } else {
          var metaNameSize = meta.getNameSize() - 1 - "meta:/".length();
          stdIoProc.printf("$meta %d %d\n", metaNameSize, Math.max(0, meta.getValueSize() - 1));
          var metaName = meta.getName().substring("meta:/".length());
          stdIoProc.println(metaName);
          stdIoProc.println(meta.getString());

          metaCopies.put(keyPointer, String.format("$copymeta %d %d\n%s\n%s", nameSize, metaNameSize, key.getName(), metaName));
        }
      }
    }
    stdIoProc.println("$end");
    stdIoProc.outputStream.flush();
  }
}
