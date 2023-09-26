package picante.math.coords;

import picante.math.vectorspace.UnwritableVectorIJ;
import picante.math.vectorspace.VectorIJ;

/**
 * This abstract class is meant to assist implementors of new coordinate types. The unwritable
 * coordinate should extend this guy. It is also meant to help ensure that the outlines for all
 * coordinates are consistent. This was also done, since the Jacobian classes require interaction
 * with the Coordinates as VectorIJs, specifically for calling mxv on a coordinate. This makes that
 * possible. There may be further times where a coordinate needs to be used as a {@link VectorIJ},
 * as long as this occurs in this package, it is possible.
 * 
 * The fact that a {@link VectorIJ} is the true composition class of all coordinate systems should
 * not leave this package. We don't want different coordinate systems to be VectorIJs as you wont
 * know what coordinate system it is supposed to be.
 * 
 * Also, if any other methods are needed on every coordinate class, they can hopefully just be added
 * in here.
 * 
 * @author G.K.Stephens
 */
abstract class AbstractVectorIJ {

  // The knowledge that this guy exists should never escape the package.
  private final UnwritableVectorIJ ijCoordinate;

  /**
   * Constructs a coordinate from the three basic components.
   */
  public AbstractVectorIJ(double i, double j) {
    this.ijCoordinate = new UnwritableVectorIJ(i, j);
  }

  // These six methods should be wrapped in new methods with the appropriate
  // names. The getters in the unwritable and the setters in the writable.
  // These six methods should never have there visibility upgraded, but should
  // be wrapped.
  /**
   * THIS METHOD SHOULD NOT BE PUBLIC
   */
  double getI() {
    return this.ijCoordinate.getI();
  }

  /**
   * THIS METHOD SHOULD NOT BE PUBLIC
   */
  double getJ() {
    return this.ijCoordinate.getJ();
  }

  /**
   * THIS METHOD SHOULD NOT BE PUBLIC
   */
  // This method should never have its visibility upgraded.
  UnwritableVectorIJ getVectorIJ() {
    return this.ijCoordinate;
  }

  /*
   * (non-Javadoc)
   * 
   * @see java.lang.Object#hashCode()
   */
  @Override
  public final int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((ijCoordinate == null) ? 0 : ijCoordinate.hashCode());
    return result;
  }

  /*
   * (non-Javadoc)
   * 
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public final boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (!this.getClass().isInstance(obj)) {
      return false;
    }
    AbstractVectorIJ other = (AbstractVectorIJ) obj;
    if (ijCoordinate == null) {
      if (other.ijCoordinate != null) {
        return false;
      }
    } else if (!ijCoordinate.equals(other.ijCoordinate)) {
      return false;
    }
    return true;
  }

  @Override
  abstract public String toString();

}
