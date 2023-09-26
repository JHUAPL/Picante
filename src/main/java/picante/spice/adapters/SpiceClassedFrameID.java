package picante.spice.adapters;

import picante.mechanics.FrameID;
import picante.spice.kernel.tk.fk.FrameType;

/**
 * Implementation of the <code>FrameID</code> interface to capture SPICE based reference frames but
 * based purely on their class and classID. These are distinct from {@link SpiceFrameID} because
 * they are created when there is no directly linkage between
 */
public final class SpiceClassedFrameID implements FrameID {

  private final FrameType type;
  private final int classID;
  private final String name;
  private final boolean inertial;

  public SpiceClassedFrameID(FrameType type, int classID) {
    this(type, classID, false);
  }

  public SpiceClassedFrameID(FrameType type, int classID, boolean inertial) {
    this.type = type;
    this.classID = classID;
    this.inertial = inertial;
    this.name = "SPICE " + type + " Frame[" + classID + "]";
  }

  public int getClassID() {
    return classID;
  }

  public FrameType getType() {
    return type;
  }

  @Override
  public String toString() {
    return name;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public boolean isInertial() {
    return inertial;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + classID;
    result = prime * result + ((type == null) ? 0 : type.hashCode());
    return result;
  }

  /**
   * {@inheritDoc}
   * 
   * The string and inertial boolean fields are not considered in the equality. The string field is
   * only a convenience, and the inertial boolean is merely a performance hint.
   */
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
    SpiceClassedFrameID other = (SpiceClassedFrameID) obj;
    if (classID != other.classID) {
      return false;
    }
    if (type != other.type) {
      return false;
    }
    return true;
  }

}
