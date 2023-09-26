package picante.mechanics.rotations;

import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.StateTransform;
import picante.mechanics.UnwritableStateTransform;

public class DifferentiatedRotations {

  /**
   * Static utility method umbrella can not be instantiated.
   */
  private DifferentiatedRotations() {}

  /**
   * Sets a receiving rotation to the value of a source rotation.
   * 
   * @param source the source rotation
   * @param receiver the rotation to receive the source's configuration
   * 
   * @return a reference to receiver for method chaining convenience
   */
  public static <U extends UnwritableDifferentiatedRotation, R extends DifferentiatedRotation> R setTo(
      U source, R receiver) {
    receiver.setTo(source.getTransform(new StateTransform()));
    return receiver;
  }

  /**
   * Computes the angular velocity vector of a state transform in the from frame
   * 
   * @param transform the transform of interest
   * 
   * @return a newly created vector that holds the angular rate in radians/time unit
   */
  public static VectorIJK getAngularVelocityInFromFrame(UnwritableStateTransform transform) {
    return new WrapperWithRate<MatrixWrapper>(new MatrixWrapper(new RotationMatrixIJK()), transform)
        .getRate();
  }

  /**
   * Computes the angular velocity vector of a differentiated rotation in the from frame
   * 
   * @param rotation the differentiated rotation of interest
   * 
   * @return a newly created vector that holds the angular rate in radians/time unit
   */
  public static VectorIJK getAngularVelocityInFromFrame(UnwritableDifferentiatedRotation rotation) {
    return getAngularVelocityInFromFrame(rotation.getTransform(new StateTransform()));
  }

  /**
   * Computes the angular velocity vector of a state transform in the to frame
   * 
   * @param transform the transform of interest
   * 
   * @return a newly created vector that holds the angular rate in radians/time unit
   */
  public static VectorIJK getAngularVelocityInToFrame(UnwritableStateTransform transform) {
    return transform.getRotation().mxv(
        new WrapperWithRate<MatrixWrapper>(new MatrixWrapper(new RotationMatrixIJK()), transform)
            .getRate());
  }

  /**
   * Computes the angular velocity vector of a differentiated rotation in the to frame
   * 
   * @param rotation the differentiated rotation of interest
   * 
   * @return a newly created vector that holds the angular rate in radians/time unit
   */
  public static VectorIJK getAngularVelocityInToFrame(UnwritableDifferentiatedRotation rotation) {
    return getAngularVelocityInToFrame(rotation.getTransform(new StateTransform()));
  }

}
