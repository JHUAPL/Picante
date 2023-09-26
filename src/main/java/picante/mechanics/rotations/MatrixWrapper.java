package picante.mechanics.rotations;

import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;

/**
 * Trivial implementation of the rotation interface that wraps around a supplied rotation matrix.
 */
public class MatrixWrapper implements Rotation {

  private final RotationMatrixIJK rotation;

  /**
   * Creates a new wrapper around a newly created rotation matrix assigned to the identity rotation.
   */
  public MatrixWrapper() {
    this.rotation = new RotationMatrixIJK(RotationMatrixIJK.IDENTITY);
  }

  /**
   * Creates a new wrapper around the supplied rotation.
   * 
   * @param rotation the rotation to wrap, a reference to the value supplied is retained by the
   *        instance.
   */
  public MatrixWrapper(RotationMatrixIJK rotation) {
    this.rotation = rotation;
  }

  /**
   * Creates a new wrapper around a newly created rotation matrix containing the contents of the
   * matrix provided by wrapper.
   * 
   * @param wrapper the wrapper from which to copy the contents
   */
  public MatrixWrapper(MatrixWrapper wrapper) {
    this.rotation = new RotationMatrixIJK(wrapper.rotation);
  }

  /**
   * Retrieve the rotation matrix.
   * <p>
   * There really is no difference between this method and the get method from the rotation
   * interface, except that this method does not perform a copy.
   * </p>
   * 
   * @return a reference to the internally held rotation matrix.
   */
  public RotationMatrixIJK get() {
    return this.rotation;
  }

  @Override
  public Rotation setTo(UnwritableRotationMatrixIJK matrix) {
    rotation.setTo(matrix);
    return this;
  }

  @Override
  public RotationMatrixIJK getRotation(RotationMatrixIJK buffer) {
    buffer.setTo(this.rotation);
    return buffer;
  }

  @Override
  public final int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((rotation == null) ? 0 : rotation.hashCode());
    return result;
  }

  @Override
  public final boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (!(obj instanceof MatrixWrapper)) {
      return false;
    }
    MatrixWrapper other = (MatrixWrapper) obj;
    if (rotation == null) {
      if (other.rotation != null) {
        return false;
      }
    } else if (!rotation.equals(other.rotation)) {
      return false;
    }
    return true;
  }

  @Override
  public String toString() {
    return rotation.toString();
  }



}
