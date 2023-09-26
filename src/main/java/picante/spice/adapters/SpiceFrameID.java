package picante.spice.adapters;

import picante.mechanics.FrameID;

/**
 * Implementation of the <code>FrameID</code> interface to capture SPICE based reference frames.
 */
public final class SpiceFrameID implements FrameID {

  private static final String PREFIX = "SPICE Frame";

  private final int idCode;
  private final String name;
  private final boolean inertial;

  /**
   * Create an non-inertial frame ID from the specified SPICE ID code.
   * 
   * @param idCode the SPICE frame ID code
   */
  public SpiceFrameID(int idCode) {
    this(idCode, false);
  }

  /**
   * Create a frame ID from the specified SPICE ID code.
   * 
   * @param idCode the SPICE frame ID code
   * @param inertial a boolean indicating whether the frame is inertial or not
   */
  public SpiceFrameID(int idCode, boolean inertial) {
    this(idCode, inertial, PREFIX + "[" + idCode + "]");
  }

  /**
   * Create a frame ID from the specified ID code with the supplied name.
   * 
   * @param idCode the SPICE frame ID code
   * @param inertial a boolean indicating whether the frame is inertial or not
   * @param name the name to associate with the ID code
   */
  SpiceFrameID(int idCode, boolean inertial, String name) {
    this.idCode = idCode;
    this.inertial = inertial;
    this.name = name;
  }

  /**
   * Retrieves the SPICE frame ID code associated with the frame
   * 
   * @return an integer containing the SPICE frame ID code
   */
  public int getIDCode() {
    return idCode;
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
    return name;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + idCode;
    return result;
  }

  @Override
  /**
   * {@inheritDoc}
   * 
   * The string and inertial boolean fields are not considered in the equality. The string field is
   * only a convenience, and the inertial boolean merely a performance hint.
   */
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
    final SpiceFrameID other = (SpiceFrameID) obj;
    if (idCode != other.idCode) {
      return false;
    }
    return true;
  }

}
