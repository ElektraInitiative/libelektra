package org.libelektra;

public enum ErrorCode {
  RESOURCE("C01100", "Resource"),
  OUT_OF_MEMORY("C01110", "Out of Memory"),
  INSTALLATION("C01200", "Installation"),
  INTERNAL("C01310", "Internal"),
  INTERFACE("C01320", "Interface"),
  PLUGIN_MISBEHAVIOR("C01330", "Plugin Misbehavior"),
  CONFLICTING_STATE("C02000", "Conflict"),
  VALIDATION_SYNTACTIC("C03100", "Validation Syntactic"),
  VALIDATION_SEMANTIC("C03200", "Validation Semantic");

  private final String number;
  private final String description;

  ErrorCode(String number, String description) {
    this.number = number;
    this.description = description;
  }

  public String getNumber() {
    return number;
  }

  public String getDescription() {
    return this.description;
  }
}
