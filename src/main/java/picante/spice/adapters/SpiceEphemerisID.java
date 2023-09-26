package picante.spice.adapters;

import picante.mechanics.EphemerisID;

/**
 * Implementation of the <code>EphemerisID</code> interface to capture SPICE based ephemeris
 * objects.
 */
public final class SpiceEphemerisID implements EphemerisID {

  private static final String PREFIX = "SPICE Object";

  private final int idCode;
  private final String name;

  /**
   * Create an ephemeris ID from the specified SPICE ID code.
   * 
   * @param idCode the SPICE ephemeris object ID code
   */
  public SpiceEphemerisID(int idCode) {
    this.idCode = idCode;
    this.name = PREFIX + "[" + idCode + "]";
  }

  /**
   * Create an ephemeris ID from the specified SPICE ID code with the supplied name.
   * 
   * @param idCode the SPICE ephemeris object ID code
   * @param name the name to associated with the ID code
   */
  public SpiceEphemerisID(int idCode, String name) {
    this.idCode = idCode;
    this.name = name;
  }

  /**
   * Retrieves the SPICE ephemeris object ID code associated with this ID.
   * 
   * @return an integer containing the SPICE ephemeris ID code
   */
  public int getIDCode() {
    return idCode;
  }

  @Override
  public String getName() {
    return name;
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

  /**
   * {@inheritDoc}
   * 
   * The string field is not considered in the equality of the ephemeris IDs.
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
    final SpiceEphemerisID other = (SpiceEphemerisID) obj;
    if (idCode != other.idCode) {
      return false;
    }
    return true;
  }

}
