package picante.mechanics.rotations;

import picante.mechanics.UnwritableStateTransform;

/**
 * Interface describing the two, minimal methods to implement a rotation and its derivative.
 */
public interface DifferentiatedRotation extends UnwritableDifferentiatedRotation {

  /**
   * Set the representation to the value of the supplied transform
   * 
   * @param transform the state transform to capture
   * 
   * @return a reference to the instance for convenience
   */
  public DifferentiatedRotation setTo(UnwritableStateTransform transform);

  /**
   * {@inheritDoc}
   * 
   * @return a writable view of the rotation, backed by the internals
   */
  @Override
  public Rotation getRotation();

}
