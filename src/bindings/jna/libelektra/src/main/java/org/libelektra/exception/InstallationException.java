package org.libelektra.exception;

import java.util.List;
import org.libelektra.ErrorCode;
import org.libelektra.KDBException;
import org.libelektra.Key;

public class InstallationException extends PermanentException {
  private static final long serialVersionUID = 1L;
  public static final String ERROR_NUMBER = ErrorCode.INSTALLATION.getNumber();

  public InstallationException(Key k) {
    super(k);
  }

  /**
   * @return The complete error information in a String with config file, mount point and debug
   *     information as it would be printed in the terminal
   * @throws IllegalStateException if this error key backing this {@link KDBException} has already
   *     been released
   */
  @Override
  public String getMessage() {
    StringBuilder builder = new StringBuilder();
    builder.append(super.getMessage());
    List<WarningEntry> warnings = getWarnings();
    if (!warnings.isEmpty()) {
      builder.append("\n").append("\n").append("Warnings:").append("\n");
      for (int i = 0; i < warnings.size(); i++) {
        WarningEntry warning = warnings.get(i);
        builder
            .append("  ")
            .append(
                String.format(
                    "#%d: Warning %s from module %s",
                    i, warning.getWarningNumber(), warning.getModule()))
            .append("\n");
        builder.append("    ").append(warning.getReason()).append("\n");
        builder.append("    ").append(MSG_CONFIGFILE).append(warning.getConfigFile()).append("\n");
        builder.append("    ").append(MSG_MOUNTPOINT).append(warning.getMountpoint()).append("\n");
        builder.append("    ").append(warning.getDebugInformation()).append("\n");
      }
    }
    return builder.toString();
  }
}
