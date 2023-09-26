package picante.spice.kernel.ck;

import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.rotations.AxisAndAngle;

/**
 * Class encapsulating the type 3 record interpolation algorithm.
 */
class Type3Interpolator {

  private AxisAndAngle axisAngle = new AxisAndAngle();

  /**
   * Interpolate between two rotation matrices at given times, assuming a constant angular rate
   * rotation from priorMatrix to afterMatrix.
   * 
   * @param priorTime the time at which priorMatrix is valid
   * @param priorMatrix a rotation matrix
   * @param afterTime the time at which afterMatrix is valid
   * @param afterMatrix a rotation matrix
   * @param time the time at which to interpolate
   * @param buffer a buffer to receive the results
   * 
   * @return a reference to the supplied buffer for convenience
   */
  public RotationMatrixIJK interpolate(double priorTime, UnwritableRotationMatrixIJK priorMatrix,
      double afterTime, UnwritableRotationMatrixIJK afterMatrix, double time,
      RotationMatrixIJK buffer) {

    double fraction = computeTimeFraction(priorTime, afterTime, time);

    /*
     * Find the matrix that rotates from the orientation specified by priorMatrix to that of
     * afterMatrix.
     */
    RotationMatrixIJK.mtxm(afterMatrix, priorMatrix, buffer);

    /*
     * Now decompose the resultant rotation into an axis and angle.
     */
    axisAngle.setTo(buffer);
    axisAngle.setAngle(axisAngle.getAngle() * fraction);
    axisAngle.getRotation(buffer);

    return RotationMatrixIJK.mxmt(priorMatrix, buffer, buffer);
  }

  /**
   * Linearly interpolate two angular rate vectors at specified times.
   * 
   * @param priorTime the time associated with priorAV
   * @param priorAV an angular rate vector
   * @param afterTime the time associated with afterAV
   * @param afterAV an angular rate vector
   * @param time the time at which to interpolate
   * @param buffer a buffer to receive the results
   * 
   * @return a reference to the supplied buffer for convenience
   */
  public VectorIJK interpolate(double priorTime, UnwritableVectorIJK priorAV, double afterTime,
      UnwritableVectorIJK afterAV, double time, VectorIJK buffer) {

    double fraction = computeTimeFraction(priorTime, afterTime, time);

    /*
     * Just linearly interpolate the angular rate vectors.
     */
    return VectorIJK.combine(1.0 - fraction, priorAV, fraction, afterAV, buffer);

  }

  /**
   * Compute the ellapsed fraction of time from priorTime to afterTime for the supplied time. That
   * is:
   * 
   * <pre>
   *       (time - priorTime) 
   *    -------------------------
   *     (afterTime - priorTime)
   * </pre>
   * 
   * @param priorTime a double representing a time
   * @param afterTime another double representing a time
   * @param time a time of interest, generally between priorTime and afterTime
   * 
   * @return the fraction of time ellapsed from priorTime to afterTime.
   */
  private double computeTimeFraction(double priorTime, double afterTime, double time) {
    return (time - priorTime) / (afterTime - priorTime);
  }

}
