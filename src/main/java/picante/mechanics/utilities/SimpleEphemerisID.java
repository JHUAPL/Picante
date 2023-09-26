package picante.mechanics.utilities;

import picante.mechanics.EphemerisID;

/**
 * Simple implementation of the {@link EphemerisID} interface.
 * <p>
 * This class overrides {@link Object#equals(Object)} and {@link Object#hashCode()} performing the
 * comparison on the name supplied to the constructor.
 * </p>
 */
public final class SimpleEphemerisID implements EphemerisID {

  /**
   * The name of the object.
   */
  private final String name;

  public SimpleEphemerisID(String name) {
    this.name = name;
  }

  @Override
  public String getName() {
    return name;
  }

  @Override
  public String toString() {
    return "SimpleEphemerisID[" + name + "]";
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
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
    SimpleEphemerisID other = (SimpleEphemerisID) obj;
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
