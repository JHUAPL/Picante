package picante.mechanics.rotations;

import static com.google.common.base.Preconditions.checkArgument;
import static picante.math.PicanteMath.PI;
import static picante.math.PicanteMath.abs;
import com.google.common.base.Optional;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;
import picante.units.FundamentalPhysicalConstants;

public class Rotations {

  /**
   * Static utility method umbrella, no need to instantiate.
   */
  private Rotations() {}

  /**
   * Sets a receiving rotation to the value of a source rotation.
   * 
   * @param source the source rotation
   * @param receiver the rotation to receive the source's configuration
   * 
   * @return a reference to receiver for convenience of method chaining
   */
  public static <U extends UnwritableRotation, R extends Rotation> R setTo(U source, R receiver) {
    receiver.setTo(source.getRotation(new RotationMatrixIJK()));
    return receiver;
  }

  /**
   * Retrieves the axis of rotation from the supplied matrix.
   * 
   * @param matrix the matrix
   * 
   * @return a newly created vector containing the axis of rotation
   */
  public static VectorIJK getRotationAxis(UnwritableRotationMatrixIJK matrix) {
    return new AxisAndAngle(matrix).getAxis();
  }

  /**
   * Retrieves the axis of rotation from the supplied rotation.
   * 
   * @param r the rotation
   * 
   * @return a newly created vector containing the axis of rotation
   */
  public static VectorIJK getRotationAxis(UnwritableRotation r) {
    return getRotationAxis(r.getRotation(new RotationMatrixIJK()));
  }

  /**
   * Retrieves the rotation angle from the supplied matrix
   * 
   * @param matrix the matrix
   * 
   * @return the rotation angle in radians. It will be in the range [0,Math.PI].
   */
  public static double getRotationAngle(UnwritableRotationMatrixIJK matrix) {
    return new AxisAndAngle(matrix).getAngle();
  }

  /**
   * Retrieves the rotation angle from the supplied rotation.
   * 
   * @param r the rotation
   * 
   * @return the rotation angle in radians. It will lie in the range [0,Math.PI].
   */
  public static double getRotationAngle(UnwritableRotation r) {
    return getRotationAngle(r.getRotation(new RotationMatrixIJK()));
  }

  /**
   * Computes an approximation to the unweighted average of rotations that are in proximity to one
   * another.
   * <p>
   * The approximation implemented here introduces errors at the 1% level if the rotations are
   * separated by less than 40 degrees in total.
   * </p>
   * <p>
   * The algorithm here is largely due to equation (14) in: C. Gramkow, &quot;On Averaging
   * Rotations&quot; Journal of Mathematical Imaging and Vision 15: 7-16, 2001
   * </p>
   * 
   * @param rotations a sequence of rotations to average. They should be in proximity to one
   *        another, or this algorithm fails.
   * @param buffer a buffer to receive the average rotation
   * 
   * @return reference to buffer for convenience
   * 
   * @see <a href="doc-files/on_averaging_rotations.pdf">On Averaging Rotations</a>
   */
  public static <U extends UnwritableRotation, R extends Rotation> R proximateAverage(
      Iterable<U> rotations, R buffer) {

    Quaternion q = new Quaternion();

    double s = 0;
    VectorIJK v = new VectorIJK(VectorIJK.ZERO);

    RotationMatrixIJK r = new RotationMatrixIJK();
    VectorIJK u = new VectorIJK();

    for (UnwritableRotation rotation : rotations) {

      /*
       * Convert rotation into quaternion format.
       */
      q.setTo(rotation.getRotation(r));

      /*
       * Now compute the sum of the quaternion components. This is all we need to track per the
       * reference paper.
       */
      s += q.getScalar();
      VectorIJK.add(v, q.getVector(u), v);

    }

    /*
     * Stuff the highly unnormalized quaternion into q. Since the setTo method normalizes the
     * quaternion, this is precisely what the reference prescribes.
     */
    q.setTo(s, v.getI(), v.getJ(), v.getK());

    /*
     * Mutate the contents of the output buffer rotation to match the rotation associated with the
     * average quaternion.
     */
    buffer.setTo(q.getRotation(r));

    return buffer;
  }

  /**
   * Computes an approximation to the unweighted average of rotation matrices that are in proximity
   * to one another.
   * <p>
   * The approximation implemented here introduces errors at the 1% level if the rotations are
   * separated by less than 40 degrees in total.
   * </p>
   * <p>
   * The algorithm here is largely due to equation (14) in: C. Gramkow, &quot;On Averaging
   * Rotations&quot; Journal of Mathematical Imaging and Vision 15: 7-16, 2001
   * </p>
   * 
   * @param matrices a sequence of rotation matrices to average. They should be in proximity to one
   *        another, or this algorithm fails.
   * @param buffer a buffer to receive the average rotation matrix
   * 
   * @return reference to buffer for convenience
   * 
   * @see <a href="doc-files/on_averaging_rotations.pdf">On Averaging Rotations</a>
   */
  public static RotationMatrixIJK proximateAverage(
      Iterable<? extends UnwritableRotationMatrixIJK> matrices, RotationMatrixIJK buffer) {

    Quaternion q = new Quaternion();

    double s = 0;
    VectorIJK v = new VectorIJK(VectorIJK.ZERO);
    VectorIJK u = new VectorIJK();

    for (UnwritableRotationMatrixIJK m : matrices) {

      /*
       * Convert the rotation into quaternion format.
       */
      q.setTo(m);

      /*
       * Now compute the sum of the quaternion components. This is all that needs to be tracked per
       * the reference paper.
       */
      s += q.getScalar();
      VectorIJK.add(v, q.getVector(u), v);

    }

    /*
     * Stuff the unnormalized components back into the quaternion q. Since setTo normalizes on
     * entry, this is precisely what the reference prescribes.
     */
    q.setTo(s, v.getI(), v.getJ(), v.getK());

    return q.getRotation(buffer);

  }


  /**
   * Compute an adjusted angular rate vector that enforces continuity of the rotation between the
   * two supplied matrices, and takes the number of rotations the input vector projected onto the
   * rotation axis suggests.
   * 
   * @param startTime time value associated with startMatrix
   * @param startMatrix the initial orientation
   * @param rateToAdjust the angular rate to adjust for continuity (expressed in radians per unit
   *        time)
   * @param finalTime time value associated with finalMatrix
   * @param finalMatrix the final orientation
   * @param tolerance an acceptance tolerance for the adjustment
   * 
   * @return an optional that contains the adjusted angular rate vector, or absent if the correction
   *         to the projection of rateToAdjust onto the rotation axis from startMatrix to
   *         finalMatrix exceeds tolerance
   */
  public static Optional<VectorIJK> computeAdjustedAngularRate(double startTime,
      UnwritableRotationMatrixIJK startMatrix, UnwritableVectorIJK rateToAdjust, double finalTime,
      UnwritableRotationMatrixIJK finalMatrix, double tolerance) {

    checkArgument(tolerance > 0.0 && tolerance <= FundamentalPhysicalConstants.HALFPI,
        "Specified tolerance: %s is outside the range: [0, Math.PI/2]", tolerance);

    AxisAndAngle aa = new AxisAndAngle();
    RotationMatrixIJK buffer = new RotationMatrixIJK();

    /*
     * Compute the matrix that rotates from the orientation specified by priorMatrix to that of
     * afterMatrix.
     */
    RotationMatrixIJK.mtxm(finalMatrix, startMatrix, buffer);

    /*
     * Decompose this into a rotation axis that rotates from prior to after.
     */
    aa.setTo(buffer);

    /*
     * Retrieve the axis, and project priorRecord's angular velocity onto it. The z-component of
     * this vector will be the actual angular rate.
     */
    VectorIJK axis = aa.getAxis(new VectorIJK());
    VectorIJK result = VectorIJK.project(rateToAdjust, axis);

    double angle = aa.getAngle();

    /*
     * The AxisAndAngle class always provides an axis associated with the smallest angular rotation
     * required to reach the rotation of interest, and as such the angle lies in [0,180]. However,
     * this code is designed to capture a rotating frame. So if <pRate,axis> is negative, then this
     * implies that the rotation lies in the [180,360] range. Correct this by subtracting the angle
     * from 360.
     */
    if (result.getDot(axis) < 0.0) {
      angle = PI * 2.0 - angle;
    }

    /*
     * Now consider the difference between the angular rate propagated value and the actual rotation
     * angle. If this difference exceeds the initialDifferenceTolerance, then stop as these two
     * records are not sufficient for propagating.
     */
    double propagatedAngle = result.getLength() * (finalTime - startTime);

    double difference = propagatedAngle % (PI * 2.0) - angle;

    /*
     * Return the zero vector if the differences between the left propagated orientation and the
     * right recorded orientation are sufficiently large.
     */
    if (abs(difference) > tolerance) {
      return Optional.absent();
    }

    /*
     * Construct the corrected angular rate to enforce continuity.
     */
    double newRate = (propagatedAngle - difference) / (finalTime - startTime);

    result.unitize().scale(newRate);

    return Optional.of(result);

  }

}
