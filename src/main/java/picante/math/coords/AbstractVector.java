package picante.math.coords;

import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

/**
 * This abstract class is meant to assist implementors of new coordinate types. The unwritable
 * coordinate should extend this guy. It is also meant to help ensure that the outlines for all
 * coordinates are consistent. This was also done, since the Jacobian classes require interaction
 * with the Coordinates as VectorIJKs, specifically for calling mxv on a coordinate. This makes that
 * possible. There may be further times where a coordinate needs to be used as a {@link VectorIJK},
 * as long as this occurs in this package, it is possible.
 * 
 * The fact that a {@link VectorIJK} is the true composition class of all coordinate systems should
 * not leave this package. We don't want different coordinate systems to be VectorIJKs as you wont
 * know what coordinate system it is supposed to be.
 * 
 * Also, if any other methods are needed on every coordinate class, they can hopefully just be added
 * in here.
 * 
 * @author G.K.Stephens
 */
abstract class AbstractVector {

  // The knowledge that this guy exists should never escape the package.
  private final UnwritableVectorIJK ijkCoordinate;

  /**
   * Constructs a coordinate from the three basic components.
   */
  public AbstractVector(double i, double j, double k) {
    this.ijkCoordinate = new UnwritableVectorIJK(i, j, k);
  }

  // These six methods should be wrapped in new methods with the appropriate
  // names. The getters in the unwritable and the setters in the writable.
  // These six methods should never have there visibility upgraded, but should
  // be wrapped.
  /**
   * THIS METHOD SHOULD NOT BE PUBLIC
   */
  double getI() {
    return this.ijkCoordinate.getI();
  }

  /**
   * THIS METHOD SHOULD NOT BE PUBLIC
   */
  double getJ() {
    return this.ijkCoordinate.getJ();
  }

  /**
   * THIS METHOD SHOULD NOT BE PUBLIC
   */
  double getK() {
    return this.ijkCoordinate.getK();
  }

  /**
   * THIS METHOD SHOULD NOT BE PUBLIC
   */
  // This method should never have its visibility upgraded.
  UnwritableVectorIJK getVectorIJK() {
    return this.ijkCoordinate;
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
    result = prime * result + ((ijkCoordinate == null) ? 0 : ijkCoordinate.hashCode());
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
    AbstractVector other = (AbstractVector) obj;
    if (ijkCoordinate == null) {
      if (other.ijkCoordinate != null) {
        return false;
      }
    } else if (!ijkCoordinate.equals(other.ijkCoordinate)) {
      return false;
    }
    return true;
  }

  @Override
  abstract public String toString();

}
