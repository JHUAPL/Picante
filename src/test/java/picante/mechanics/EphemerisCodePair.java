package picante.mechanics;

/**
 * A class that connects two of the EphemerisID instances to support map operations of functions
 * loaded into a test ephemeris provider.
 */
public class EphemerisCodePair {

  EphemerisTestCodes targetID;
  EphemerisTestCodes observerID;

  public EphemerisCodePair(EphemerisTestCodes targetID, EphemerisTestCodes observerID) {
    this.targetID = targetID;
    this.observerID = observerID;
  }

  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((observerID == null) ? 0 : observerID.hashCode());
    result = prime * result + ((targetID == null) ? 0 : targetID.hashCode());
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
    final EphemerisCodePair other = (EphemerisCodePair) obj;
    if (observerID == null) {
      if (other.observerID != null) {
        return false;
      }
    } else if (!observerID.equals(other.observerID)) {
      return false;
    }
    if (targetID == null) {
      if (other.targetID != null) {
        return false;
      }
    } else if (!targetID.equals(other.targetID)) {
      return false;
    }
    return true;
  }

}
