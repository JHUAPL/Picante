package picante.mechanics.rotations;

import picante.math.vectorspace.RotationMatrixIJK;

/**
 * Interface encapsulating rotation matrix retrieval from another class.
 */
public interface UnwritableRotation {

  // TODO: This default method would be useful, but conflicts with WrapperWithRate#getRotation().
  // /**
  // * Convert the representation of the rotation into a matrix.
  // *
  // * @return newly created RotationMatrixIJK containing the result
  // */
  // default public RotationMatrixIJK getRotation() {
  // return getRotation(new RotationMatrixIJK());
  // }

  /**
   * Convert the representation of the rotation into a matrix.
   * 
   * @param buffer the buffer to capture the result
   * 
   * @return a reference to buffer for convenience
   */
  public RotationMatrixIJK getRotation(RotationMatrixIJK buffer);

}
