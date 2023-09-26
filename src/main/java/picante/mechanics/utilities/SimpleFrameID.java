package picante.mechanics.utilities;

import picante.mechanics.FrameID;

/**
 * Simple implementation of the {@link FrameID} interface.
 * <p>
 * Note: both the inertial and name fields retained on this class are used in the implementation of
 * {@link Object#equals(Object)} and {@link Object#hashCode()}. So care must be taken if you create
 * two frames with the same name and different inertial states, as they will be regarded as two
 * entirely different frames.
 * </p>
 */
public final class SimpleFrameID implements FrameID {

  /**
   * The name of the frame.
   */
  private final String name;

  /**
   * The inertial state, if true - inertial. False otherwise.
   */
  private final boolean inertial;

  /**
   * Creates a simple, non-inertial frame ID.
   * 
   * @param name the name of the frame
   */
  public SimpleFrameID(String name) {
    this(name, false);
  }

  /**
   * Creates a simple frame ID.
   * 
   * @param name the name of the frame
   * @param inertial whether the frame is inertial or not.
   */
  public SimpleFrameID(String name, boolean inertial) {
    this.name = name;
    this.inertial = inertial;
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
  public String toString() {
    return "SimpleFrameID[" + name + (inertial ? ", inertial" : "") + "]";
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + (inertial ? 1231 : 1237);
    result = prime * result + ((name == null) ? 0 : name.hashCode());
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
    SimpleFrameID other = (SimpleFrameID) obj;
    if (inertial != other.inertial) {
      return false;
    }
    if (name == null) {
      if (other.name != null) {
        return false;
      }
    } else if (!name.equals(other.name)) {
      return false;
    }
    return true;
  }

}
