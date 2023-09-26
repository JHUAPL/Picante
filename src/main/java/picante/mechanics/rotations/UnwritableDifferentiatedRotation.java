package picante.mechanics.rotations;

import picante.mechanics.StateTransform;

/**
 * Interface capturing state transformation retrieval from another class.
 */
public interface UnwritableDifferentiatedRotation {

  /**
   * Convert the representation of a rotation and its derivative into a state transformation.
   * 
   * @param buffer the state transform to capture the result
   * 
   * @return a reference to buffer for convenience
   */
  public StateTransform getTransform(StateTransform buffer);

  /**
   * Returns an unwritable rotation view of this differentiated rotation.
   * 
   * @return a view, possibly newly created but not required, backed by the internals of this
   *         implementation
   * 
   */
  public UnwritableRotation getRotation();

}
