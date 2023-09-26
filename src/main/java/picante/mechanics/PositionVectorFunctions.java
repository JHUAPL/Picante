package picante.mechanics;

import picante.math.functions.VectorIJKFunction;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

/**
 * Class capturing static utility methods useful for manipulating and working with
 * {@link PositionVectorFunction}s.
 */
public class PositionVectorFunctions {

  /**
   * Static utility method umbrella, can not be instantiated.
   */
  private PositionVectorFunctions() {}

  /**
   * Adapts the supplied position vector function to the {@link VectorIJKFunction} interface.
   * 
   * @param function the function to adapt
   * 
   * @return an adapter that passes the time argument through to the
   *         {@link PositionVectorFunction#getPosition(double, VectorIJK)} method.
   */
  public static VectorIJKFunction adapt(final PositionVectorFunction function) {
    return new VectorIJKFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return function.getPosition(t, buffer);
      }
    };
  }

  /**
   * Creates a new function from the source by applying a scale factor to the returned position
   * vector.
   * 
   * @param function the function to scale
   * @param scale the positive scale factor to apply to function's evaluations
   * 
   * @return a newly created function that wraps the supplied function and scales the output on
   *         evaluation
   * 
   * @throws IllegalArgumentException if scale is not strictly positive.
   */
  public static PositionVectorFunction scale(final PositionVectorFunction function,
      final double scale) {

    /*
     * Check to see if the scale argument is less than zero, if it is throw an illegal argument
     * exception as this method is only designed to apply positive scals to the function.
     */
    if (scale <= 0.0) {
      throw new IllegalArgumentException(
          "Scale factor: " + scale + " is invalid, as it is less than or equal to 0.0");
    }

    return new AbstractPositionVectorFunctionWrapper(function) {
      @Override
      public VectorIJK getPosition(double time, VectorIJK buffer) {
        buffer = buffer == null ? new VectorIJK() : buffer;
        function.getPosition(time, buffer);
        return buffer.scale(scale);
      }
    };
  }

  /**
   * Creates a new function from the source by negating the vector.
   * <p>
   * As a consequence of this negation, the observer and target ID fields are swapped.
   * </p>
   * 
   * @param function the function to negate
   * 
   * @return a newly created function that wraps the supplied function and evaluates to the negated
   *         position
   */
  public static PositionVectorFunction negate(final PositionVectorFunction function) {
    return new AbstractReversedPositionVectorFunctionWrapper(function) {

      @Override
      public VectorIJK getPosition(double time, VectorIJK buffer) {
        buffer = buffer == null ? new VectorIJK() : buffer;
        function.getPosition(time, buffer);
        return buffer.negate();
      }
    };

  }

  /**
   * Creates a new function from the source by applying a frame transformation.
   * <p>
   * The resultant function reports transform toID as its frameID.
   * </p>
   * 
   * @param transform the transform to apply to the function
   * @param function the function to transform
   * 
   * @return a newly created function that applies transform to the output vector
   * 
   * @throws IllegalArgumentException if transform fromID and function frameID do not match.
   */
  public static PositionVectorFunction transform(final FrameTransformFunction transform,
      final PositionVectorFunction function) {

    /*
     * Check to see that the transform is appropriate for the function provided. All this implies is
     * that the function is specified in the frame of the fromID from the transform.
     */
    if (!transform.getFromID().equals(function.getFrameID())) {
      throw new IllegalArgumentException("The frame the position vector function is specified in: "
          + function.getFrameID() + " is not equivalent to the fromID of the frame transform: "
          + transform.getFromID());
    }

    return new AbstractPositionVectorFunctionWrapper(function) {

      private final RotationMatrixIJK matrix = new RotationMatrixIJK();
      private final Coverage coverage =
          Coverages.intersect(transform.getCoverage(), function.getCoverage());

      @Override
      public FrameID getFrameID() {
        return transform.getToID();
      }

      @Override
      public Coverage getCoverage() {
        return coverage;
      }

      @Override
      public VectorIJK getPosition(double time, VectorIJK buffer) {
        buffer = buffer == null ? new VectorIJK() : buffer;
        function.getPosition(time, buffer);
        transform.getTransform(time, matrix);
        return matrix.mxv(buffer, buffer);
      }
    };
  }

  /**
   * Creates a fixed offset {@link PositionVectorFunction} valid for all time.
   * 
   * @param targetID the target ID of the head of the fixed vector
   * @param observerID the observer ID of the tail of the fixed vector
   * @param frameID the frameID of the fixed vector
   * @param vector the vector
   * 
   * @return a newly created position vector function that always evaluates to the fixed vector at
   *         any time. Note the adapter copies the supplied vector, so it is not a view.
   */
  public static PositionVectorFunction createFixed(final EphemerisID targetID,
      final EphemerisID observerID, final FrameID frameID, final UnwritableVectorIJK vector) {
    return createFixed(targetID, observerID, frameID, Coverage.ALL_TIME, vector);
  }

  /**
   * Creates a fixed offset {@link PositionVectorFunction} valid for the specified coverage.
   * <p>
   * The resultant function will generate {@link EphemerisEvaluationException} if the supplied time
   * is out of the bounds of the coverage
   * </p>
   * 
   * @param targetID the target ID of the head of the fixed vector
   * @param observerID the observer ID of the tail of the fixed vector
   * @param frameID the frameID of the fixed vector
   * @param coverage the coverage where this function is valid
   * @param vector the vector
   * 
   * @return a newly created {@link PositionVectorFunction} that always evaluates to the fixed
   *         vector at any time within the supplied coverage. Note the adapter copies the supplied
   *         vector, so it is not a view.
   */
  public static PositionVectorFunction createFixed(final EphemerisID targetID,
      final EphemerisID observerID, final FrameID frameID, final Coverage coverage,
      final UnwritableVectorIJK vector) {
    return new PositionVectorFunction() {

      private final UnwritableVectorIJK fixed = new UnwritableVectorIJK(vector);

      @Override
      public EphemerisID getTargetID() {
        return targetID;
      }

      @Override
      public VectorIJK getPosition(double time, VectorIJK buffer) {
        buffer = buffer == null ? new VectorIJK() : buffer;
        if (!coverage.contains(time)) {
          throw new EphemerisEvaluationException("Unable to evaluate function at time: " + time);
        }
        return buffer.setTo(fixed);
      }

      @Override
      public EphemerisID getObserverID() {
        return observerID;
      }

      @Override
      public FrameID getFrameID() {
        return frameID;
      }

      @Override
      public Coverage getCoverage() {
        return coverage;
      }
    };
  }

}
