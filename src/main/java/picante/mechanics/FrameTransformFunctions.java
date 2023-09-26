package picante.mechanics;

import static com.google.common.base.Preconditions.checkArgument;
import picante.math.functions.RotationMatrixIJKFunction;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;

/**
 * Class containing static utility methods for manipulating and working with
 * {@link FrameTransformFunction}s.
 */
public class FrameTransformFunctions {

  /**
   * Static utility method umbrella, can not be instantiated.
   */
  private FrameTransformFunctions() {}

  /**
   * Creates a new function from the source by inverting (transposing) the supplied transformation.
   * <p>
   * As a consequence of this inversion, the to and from ID fields are swapped.
   * </p>
   * 
   * @param function the function to invert
   * 
   * @return a newly created function that wraps the supplied function and evaluates to the
   *         transpose
   */
  public static FrameTransformFunction invert(final FrameTransformFunction function) {
    return new AbstractReversedFrameTransformFunctionWrapper(function) {

      @Override
      public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {
        return function.getTransform(time, buffer).transpose();
      }
    };
  }

  /**
   * Creates a fixed offset {@link FrameTransformFunction} valid for all time.
   * 
   * @param fromID the ID of the frame the rotation transforms from
   * @param toID the ID of the frame the rotation transforms to
   * @param rotation the fixed rotation
   * 
   * @return a newly created {@link FrameTransformFunction} that always evaluates to a fixed
   *         rotation at any time. Note the adapter copies the supplied rotation, so it is not a
   *         view.
   */
  public static FrameTransformFunction createFixed(final FrameID fromID, final FrameID toID,
      final UnwritableRotationMatrixIJK rotation) {
    return createFixed(fromID, toID, Coverage.ALL_TIME, rotation);
  }

  /**
   * Creates a fixed offset {@link FrameTransformFunction} valid for the specified coverage.
   * <p>
   * The resultant function will throw {@link FrameEvaluationException} if the supplied time lies
   * outside the supplied coverage.
   * </p>
   * 
   * @param fromID the ID of the frame the rotation transforms from
   * @param toID the ID of the frame the rotation transforms to
   * @param coverage the coverage where this function is valid
   * @param rotation the fixed rotation
   * 
   * @return a newly created {@link FrameTransformFunction} that always evaluates to the fixed
   *         rotation at times within the supplied coverage. Note this adapter copies the supplied
   *         rotation, so it is not a view.
   */
  public static FrameTransformFunction createFixed(final FrameID fromID, final FrameID toID,
      final Coverage coverage, final UnwritableRotationMatrixIJK rotation) {

    return new FrameTransformFunction() {

      private final UnwritableRotationMatrixIJK fixed = new UnwritableRotationMatrixIJK(rotation);

      @Override
      public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {
        buffer = buffer == null ? new RotationMatrixIJK() : buffer;

        if (!coverage.contains(time)) {
          throw new FrameEvaluationException(
              "Unable to evaluate frame transformation at time: " + time);
        }
        return buffer.setTo(fixed);
      }

      @Override
      public FrameID getToID() {
        return toID;
      }

      @Override
      public FrameID getFromID() {
        return fromID;
      }

      @Override
      public Coverage getCoverage() {
        return coverage;
      }
    };

  }

  /**
   * Creates a {@link RotationMatrixIJKFunction} view from the supplied
   * {@link FrameTransformFunction}.
   * 
   * @param function the function to adapt
   * @return a newly created {@link RotationMatrixIJKFunction} that wraps the supplied function
   */
  public static RotationMatrixIJKFunction adapt(final FrameTransformFunction function) {

    return new RotationMatrixIJKFunction() {

      @Override
      public RotationMatrixIJK evaluate(double t, RotationMatrixIJK buffer) {
        return function.getTransform(t, buffer);
      }
    };

  }

  /**
   * Creates a {@link FrameTransformFunction} view from the supplied
   * {@link RotationMatrixIJKFunction} valid for all time.
   * 
   * @param fromID the ID of the frame the rotation transforms from
   * @param toID the ID of the frame the rotation transforms to
   * @param function the function to adapt
   * @return a newly created {@link FrameTransformFunction} that wraps the supplied function
   */
  public static FrameTransformFunction adapt(final FrameID fromID, final FrameID toID,
      final RotationMatrixIJKFunction function) {
    return adapt(fromID, toID, Coverage.ALL_TIME, function);
  }

  /**
   * Creates a {@link FrameTransformFunction} view from the supplied
   * {@link RotationMatrixIJKFunction} valid for the specified coverage.
   * 
   * @param fromID the ID of the frame the rotation transforms from
   * @param toID the ID of the frame the rotation transforms to
   * @param coverage the coverage where this function is valid
   * @param function the function to adapt
   * @return a newly created {@link FrameTransformFunction} that wraps the supplied function
   */
  public static FrameTransformFunction adapt(final FrameID fromID, final FrameID toID,
      final Coverage coverage, final RotationMatrixIJKFunction function) {

    return new FrameTransformFunction() {

      @Override
      public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {
        if (!coverage.contains(time)) {
          throw new FrameEvaluationException(
              "Unable to evaluate frame transformation at time: " + time);
        }
        return function.evaluate(time, buffer);
      }

      @Override
      public FrameID getToID() {
        return toID;
      }

      @Override
      public FrameID getFromID() {
        return fromID;
      }

      @Override
      public Coverage getCoverage() {
        return coverage;
      }
    };
  }

  /**
   * Creates a frame transform function by multiplying a by b.
   * 
   * @param a the transform on the left of the multiplication operator
   * @param b the transform on the right of the multiplication operator
   * 
   * @return a new transform function that is the product of a with b.
   * 
   * @throws IllegalArgumentException if a's from ID is not equal to b's to ID or if the coverage
   *         associated with a and b result in an empty intersection
   */
  public static FrameTransformFunction mxm(final FrameTransformFunction a,
      final FrameTransformFunction b) {

    /*
     * Check to see that the from and to id's match up.
     */
    checkArgument((a.getFromID().equals(b.getToID())), "Incompatible frame IDs. FromID %s ToID %s",
        a.getFromID(), b.getToID());

    return new FrameTransformFunction() {

      private final Coverage coverage = Coverages.intersect(a.getCoverage(), b.getCoverage());

      @Override
      public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {

        buffer = buffer == null ? new RotationMatrixIJK() : buffer;

        RotationMatrixIJK r = new RotationMatrixIJK();
        a.getTransform(time, r);
        b.getTransform(time, buffer);

        return RotationMatrixIJK.mxm(r, buffer, buffer);
      }

      @Override
      public FrameID getToID() {
        return a.getToID();
      }

      @Override
      public FrameID getFromID() {
        return b.getFromID();
      }

      @Override
      public Coverage getCoverage() {
        return coverage;
      }
    };

  }

  public static FrameTransformFunction mxmt(final FrameTransformFunction a,
      final FrameTransformFunction b) {

    /*
     * Check to see that the from and to id's match up.
     */
    checkArgument((a.getFromID().equals(b.getFromID())),
        "Incompatible frame IDs. FromID %s FromID %s", a.getFromID(), b.getFromID());

    return new FrameTransformFunction() {

      private final Coverage coverage = Coverages.intersect(a.getCoverage(), b.getCoverage());

      @Override
      public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {

        buffer = buffer == null ? new RotationMatrixIJK() : buffer;

        RotationMatrixIJK r = new RotationMatrixIJK();
        a.getTransform(time, r);
        b.getTransform(time, buffer);

        return RotationMatrixIJK.mxmt(r, buffer, buffer);
      }

      @Override
      public FrameID getToID() {
        return a.getToID();
      }

      @Override
      public FrameID getFromID() {
        return b.getToID();
      }

      @Override
      public Coverage getCoverage() {
        return coverage;
      }
    };

  }

  public static FrameTransformFunction mtxm(final FrameTransformFunction a,
      final FrameTransformFunction b) {

    /*
     * Check to see that the from and to id's match up.
     */
    checkArgument((a.getToID().equals(b.getToID())), "Incompatible frame IDs. ToID %s ToID %s",
        a.getToID(), b.getToID());

    return new FrameTransformFunction() {

      private final Coverage coverage = Coverages.intersect(a.getCoverage(), b.getCoverage());

      @Override
      public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {

        buffer = buffer == null ? new RotationMatrixIJK() : buffer;

        RotationMatrixIJK r = new RotationMatrixIJK();
        a.getTransform(time, r);
        b.getTransform(time, buffer);

        return RotationMatrixIJK.mtxm(r, buffer, buffer);
      }

      @Override
      public FrameID getToID() {
        return a.getFromID();
      }

      @Override
      public FrameID getFromID() {
        return b.getFromID();
      }

      @Override
      public Coverage getCoverage() {
        return coverage;
      }
    };

  }
}
