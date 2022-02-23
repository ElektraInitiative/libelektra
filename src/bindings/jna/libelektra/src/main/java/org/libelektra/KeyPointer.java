package org.libelektra;

import com.sun.jna.Pointer;
import java.util.Objects;
import javax.annotation.Nullable;

public class KeyPointer {
  @Nullable private final Pointer pointer;

  public KeyPointer(ReadableKey key) {
    this.pointer = key.getPointer();
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (!(o instanceof KeyPointer)) {
      return false;
    }
    KeyPointer that = (KeyPointer) o;
    return Objects.equals(pointer, that.pointer);
  }

  @Override
  public int hashCode() {
    return Objects.hash(pointer);
  }
}
