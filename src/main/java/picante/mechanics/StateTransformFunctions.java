package picante.mechanics;

import static com.google.common.base.Preconditions.checkArgument;
import picante.math.PicanteMath;
import picante.math.functions.DifferentiableRotationMatrixIJKFunction;
import picante.math.functions.UnivariateFunction;
import picante.math.functions.UnivariateFunctions;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableRotationMatrixIJK;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.rotations.AxisAndAngle;
import picante.mechanics.rotations.MatrixWrapper;
import picante.mechanics.rotations.WrapperWithRate;

/**
 * Class containing static utility methods for manipulating and working with
 * {@link StateTransformFunction}s.
 */
public class StateTransformFunctions {

  /**
   * Static utility method umbrella, can not be instantiated.
   */
  private StateTransformFunctions() {}

  /**
   * Creates a new function from the source by inverting the supplied transformation.
   * <p>
   * As a consequence of this inversion, the to and from ID fields are swapped.
   * </p>
   * 
   * @param function the function to invert
   * 
   * @return a newly created function that wraps the supplied function and evaluates to the inverse
   */
  public static StateTransformFunction invert(final StateTransformFunction function) {
    return new AbstractReversedStateTransformFunctionWrapper(function) {

      @Override
      public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {
        return function.getTransform(time, buffer).transpose();
      }

      @Override
      public StateTransform getStateTransform(double time, StateTransform buffer) {
        return function.getStateTransform(time, buffer).invert();
      }
    };
  }

  /**
   * Creates a fixed offset {@link StateTransformFunction} valid for all time.
   * <p>
   * As the rotation is fixed, the rotation derivative components will always evaluate to
   * {@link MatrixIJK#ZEROS}.
   * </p>
   * 
   * @param fromID the ID of the frame the rotation transforms from
   * @param toID the ID of the frame the rotation transforms to
   * @param rotation the fixed rotation
   * 
   * @return a newly created {@link StateTransformFunction} that always evaluates to a fixed
   *         rotation at any time. Note the adapter copies the supplied rotation, so it is not a
   *         view.
   */
  public static StateTransformFunction createFixed(final FrameID fromID, final FrameID toID,
      final UnwritableRotationMatrixIJK rotation) {
    return createFixed(fromID, toID, Coverage.ALL_TIME, rotation);
  }

  /**
   * Creates a fixed offset {@link StateTransformFunction} valid for the specified coverage.
   * <p>
   * As the rotation is fixed, the rotation derivative components will always evaluate to
   * {@link MatrixIJK#ZEROS}.
   * </p>
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
   * @return a newly created {@link StateTransformFunction} that always evaluates to the fixed
   *         rotation at times within the supplied coverage. Note this adapter copies the supplied
   *         rotation, so it is not a view.
   */
  public static StateTransformFunction createFixed(final FrameID fromID, final FrameID toID,
      final Coverage coverage, final UnwritableRotationMatrixIJK rotation) {
    return new StateTransformFunction() {

      private final UnwritableStateTransform fixed =
          new UnwritableStateTransform(rotation, MatrixIJK.ZEROS);

      @Override
      public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {
        if (!coverage.contains(time)) {
          throw new FrameEvaluationException("Unable to evaluate frame transform at time: " + time);
        }

        return buffer.setTo(fixed.getRotation());
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

      @Override
      public StateTransform getStateTransform(double time, StateTransform buffer) {
        if (!coverage.contains(time)) {
          throw new FrameEvaluationException("Unable to evaluate state transform at time: " + time);
        }
        return buffer.setTo(fixed);
      }
    };
  }

  public static DifferentiableRotationMatrixIJKFunction adapt(final StateTransformFunction a) {
    return new DifferentiableRotationMatrixIJKFunction() {

      @Override
      public RotationMatrixIJK evaluate(double t, RotationMatrixIJK buffer) {
        return a.getTransform(t, buffer);
      }

      @Override
      public MatrixIJK differentiate(double t, MatrixIJK buffer) {
        StateTransform transform = new StateTransform();
        a.getStateTransform(t, transform);
        return buffer.setTo(transform.dRotation);
      }
    };
  }

  public static StateTransformFunction adapt(final FrameID fromID, final FrameID toID,
      final DifferentiableRotationMatrixIJKFunction function) {
    return adapt(fromID, toID, Coverage.ALL_TIME, function);
  }

  public static StateTransformFunction adapt(final FrameID fromID, final FrameID toID,
      final Coverage coverage, final DifferentiableRotationMatrixIJKFunction function) {

    return new StateTransformFunction() {

      @Override
      public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {
        buffer = buffer == null ? new RotationMatrixIJK() : buffer;
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

      @Override
      public StateTransform getStateTransform(double time, StateTransform buffer) {
        buffer = buffer == null ? new StateTransform() : buffer;
        function.evaluate(time, buffer.rotation);
        function.differentiate(time, buffer.dRotation);
        return buffer;
      }
    };
  }

  /**
   * Creates a {@link StateTransformFunction} by estimating the derivative numerically from the
   * supplied {@link FrameTransformFunction}
   * <p>
   * This function estimates the derivative of the supplied frame transform function by assuming a
   * constant angular rate of rotation between the requested time and the requested time + delta.
   * This is the value used to populate the derivative components of the {@link StateTransform}.
   * </p>
   * <p>
   * The requested value of delta (including its sign) is used everywhere it is possible to do so.
   * If time + delta lies outside the interval of coverage containing time, the code will attempt to
   * use time - delta to estimate the derivative instead.
   * </p>
   * 
   * @param f the frame transform function to differentiate into a state transform
   * @param delta the time offset used to compute the second rotation from which the angular rate is
   *        specified. Note: in general this should be small enough that the total rotation from f
   *        evaluated at time, and f evaluated at time + delta is less than {@link PicanteMath#PI}.
   *        Further the time should also be less than half the smallest interval of coverage offered
   *        by the supporting transform function f.
   * 
   * @return a newly created {@link StateTransformFunction} that delegates to the supplied function
   *         f, except for {@link StateTransformFunction#getStateTransform(double, StateTransform)}
   *         calls, where it attempts to estimate the derivative
   */
  public static StateTransformFunction estimateDerivative(FrameTransformFunction f, double delta) {
    checkArgument(delta != 0.0, "Zero values of delta are not permitted.");
    return estimateDerivative(f, UnivariateFunctions.create(delta));
  }

  /**
   * Creates a {@link StateTransformFunction} by estimating the derivative numerically from the
   * supplied {@link FrameTransformFunction}
   * <p>
   * This function estimates the derivative of the supplied frame transform function by assuming a
   * constant angular rate of rotation between the requested time and the requested time +
   * delta(time). This is the value used to populate the derivative components of the
   * {@link StateTransform}.
   * </p>
   * <p>
   * The value of delta(time) (including its sign) is used everywhere it is possible to do so. If
   * time + delta(time) lies outside the interval of coverage containing time, the code will attempt
   * to use time - delta(time) to estimate the derivative instead.
   * </p>
   * 
   * @param f the frame transform function to differentiate into a state transform
   * @param delta a function providing the time offset to compute the second rotation from which the
   *        angular rate is specified. Note: in general the range of values delta assumes should be
   *        small enough that the total rotation from f evaluated at time and f evalauted at time +
   *        delta(time) is less than {@link PicanteMath#PI}. Further, the range of times assumed by
   *        the function delta should generally be less than half the smallest interval of coverage
   *        offered by the supporting transform function f.
   * 
   * @return a newly created {@link StateTransformFunction} that delegates to the supplied function
   *         f, except for {@link StateTransformFunction#getStateTransform(double, StateTransform)}
   *         calls, where it attempts to estimate the derivative
   */
  public static StateTransformFunction estimateDerivative(final FrameTransformFunction f,
      final UnivariateFunction delta) {
    return new StateTransformFunction() {

      @Override
      public RotationMatrixIJK getTransform(double time, RotationMatrixIJK buffer) {
        return f.getTransform(time, buffer);
      }

      @Override
      public FrameID getToID() {
        return f.getToID();
      }

      @Override
      public FrameID getFromID() {
        return f.getFromID();
      }

      @Override
      public Coverage getCoverage() {
        /*
         * This isn't quite right, as coverage needs to be contracted by delta(t)...
         */
        return f.getCoverage();
      }

      @Override
      public StateTransform getStateTransform(double time, StateTransform buffer) {

        /*
         * Check to see if we can compute the transform at time+delta.
         */
        double dT = delta.evaluate(time);

        /*
         * Check to see if dT is 0. If it is, this isn't sane and let the user know so.
         */
        if (dT == 0.0) {
          throw new FrameEvaluationException("Unable to estimate derivative, time delta is 0.0");
        }

        /*
         * Try to use time+dT, otherwise try time-dT (if we hit the end of a coverage interval).
         */
        double otherTime = time + dT;
        if (!f.getCoverage().contains(otherTime)) {
          otherTime = time - dT;
          dT = -dT;
          if (!f.getCoverage().contains(otherTime)) {
            throw new FrameEvaluationException(
                "Unable to compute derivative estimate at: " + otherTime);
          }
        }

        RotationMatrixIJK atTime = f.getTransform(time, new RotationMatrixIJK());
        RotationMatrixIJK atOtherTime = f.getTransform(otherTime, new RotationMatrixIJK());

        /*
         * Compute the rotation from the fromID frame at time to fromID frame at otherTime.
         */
        RotationMatrixIJK timeToOther = RotationMatrixIJK.mtxm(atOtherTime, atTime, atOtherTime);

        /*
         * Determine the axis and angle of the timeToOther rotation. The axis will be specified in
         * the fromID frame.
         */
        AxisAndAngle aa = new AxisAndAngle(timeToOther);
        VectorIJK rate = aa.getAxis().scale(aa.getAngle() / dT);

        /*
         * Combine the evaluated matrix atTime with the newly derived rate to create the transform.
         */
        WrapperWithRate<MatrixWrapper> wrapper =
            new WrapperWithRate<MatrixWrapper>(new MatrixWrapper(atTime), rate);

        return wrapper.getTransform(buffer);
      }
    };
  }

  public static StateTransformFunction mxm(final StateTransformFunction a,
      final StateTransformFunction b) {

    /*
     * Check to see that the from and to id's match up.
     */
    checkArgument((a.getFromID().equals(b.getToID())), "Incompatible frame IDs. FromID %s ToID %s",
        a.getFromID(), b.getToID());

    return new StateTransformFunction() {

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

      @Override
      public StateTransform getStateTransform(double time, StateTransform buffer) {
        buffer = buffer == null ? new StateTransform() : buffer;
        StateTransform t = new StateTransform();
        a.getStateTransform(time, t);
        b.getStateTransform(time, buffer);

        return StateTransform.mxm(t, buffer, buffer);
      }
    };

  }

  public static StateTransformFunction mxmi(final StateTransformFunction a,
      final StateTransformFunction b) {

    /*
     * Check to see that the from and to id's match up.
     */
    checkArgument((a.getFromID().equals(b.getFromID())),
        "Incompatible frame IDs. FromID %s FromID %s", a.getFromID(), b.getFromID());

    return new StateTransformFunction() {

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

      @Override
      public StateTransform getStateTransform(double time, StateTransform buffer) {
        buffer = buffer == null ? new StateTransform() : buffer;
        StateTransform t = new StateTransform();
        a.getStateTransform(time, t);
        b.getStateTransform(time, buffer);

        return StateTransform.mxmi(t, buffer, buffer);
      }
    };

  }

  public static StateTransformFunction mixm(final StateTransformFunction a,
      final StateTransformFunction b) {

    /*
     * Check to see that the from and to id's match up.
     */
    checkArgument((a.getToID().equals(b.getToID())), "Incompatible frame IDs. ToID %s ToID %s",
        a.getToID(), b.getToID());

    return new StateTransformFunction() {

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

      @Override
      public StateTransform getStateTransform(double time, StateTransform buffer) {
        buffer = buffer == null ? new StateTransform() : buffer;
        StateTransform t = new StateTransform();
        a.getStateTransform(time, t);
        b.getStateTransform(time, buffer);

        return StateTransform.mixm(t, buffer, buffer);
      }
    };

  }

}
