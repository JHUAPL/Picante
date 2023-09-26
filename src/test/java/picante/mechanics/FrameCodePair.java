package picante.mechanics;

/**
 * A class that connects two of the FrameID instances to support map operations of functions loaded
 * into a test frame provider.
 */
public class FrameCodePair {
  FrameTestCodes fromID;
  FrameTestCodes toID;

  public FrameCodePair(FrameTestCodes fromID, FrameTestCodes toID) {
    this.fromID = fromID;
    this.toID = toID;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((fromID == null) ? 0 : fromID.hashCode());
    result = prime * result + ((toID == null) ? 0 : toID.hashCode());
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
    final FrameCodePair other = (FrameCodePair) obj;
    if (fromID == null) {
      if (other.fromID != null) {
        return false;
      }
    } else if (!fromID.equals(other.fromID)) {
      return false;
    }
    if (toID == null) {
      if (other.toID != null) {
        return false;
      }
    } else if (!toID.equals(other.toID)) {
      return false;
    }
    return true;
  }

}
