package picante.mechanics.rotations;

import picante.math.vectorspace.UnwritableRotationMatrixIJK;

/**
 * Interface describing the two, minimal methods to implement a rotation.
 */
public interface Rotation extends UnwritableRotation {

  /**
   * Set the representation to the value of the supplied matrix.
   * 
   * @param matrix the rotation matrix to capture
   * 
   * @return a reference to the instance for convenience
   */
  public Rotation setTo(UnwritableRotationMatrixIJK matrix);

}
