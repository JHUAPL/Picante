package picante.spice.kernel.tk.fk;

import picante.spice.Utilities;

public class FrameInfo {

  private final int code;
  private final int classID;
  private final int centerID;
  private final String name;
  private final FrameType type;

  /**
   * Class that aggregates the required SPICE meta data associated with a frame definition.
   */
  public FrameInfo(String name, int code, FrameType type, int classID, int centerID) {
    this.name = Utilities.canonicalizeSpiceName(name);
    this.code = code;
    this.type = type;
    this.classID = classID;
    this.centerID = centerID;
  }

  public int getCode() {
    return code;
  }

  public int getClassID() {
    return classID;
  }

  public int getCenterID() {
    return centerID;
  }

  public String getName() {
    return name;
  }

  public FrameType getType() {
    return type;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + centerID;
    result = prime * result + classID;
    result = prime * result + code;
    result = prime * result + ((name == null) ? 0 : name.hashCode());
    result = prime * result + ((type == null) ? 0 : type.hashCode());
    return result;
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    FrameInfo other = (FrameInfo) obj;
    if (centerID != other.centerID) {
      return false;
    }
    if (classID != other.classID) {
      return false;
    }
    if (code != other.code) {
      return false;
    }
    if (name == null) {
      if (other.name != null) {
        return false;
      }
    } else if (!name.equals(other.name)) {
      return false;
    }
    if (type != other.type) {
      return false;
    }
    return true;
  }

}
