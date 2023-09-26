package picante.mechanics;

import picante.math.PicanteMath;
import picante.math.functions.DifferentiableVectorIJKFunction;
import picante.math.functions.VectorIJKFunction;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.math.vectorspace.UnwritableVectorIJK;
import picante.math.vectorspace.VectorIJK;

/**
 * Class capturing static utility methods useful for manipulating and working with
 * {@link StateVectorFunction}s.
 */
public class StateVectorFunctions {

  /**
   * Static utility umbrella, can not be instantiated.
   */
  private StateVectorFunctions() {}

  /**
   * Adapts the supplied state vector function to the {@link DifferentiableVectorIJKFunction}
   * interface.
   * 
   * @param function the function to adapt
   * 
   * @return an adapter that passes the time argument through to the
   *         {@link StateVectorFunction#getPosition(double, VectorIJK)} method and
   *         {@link StateVectorFunction#getState(double, StateVector)}.
   */
  public static DifferentiableVectorIJKFunction adapt(final StateVectorFunction function) {
    return new DifferentiableVectorIJKFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return function.getPosition(t, buffer);
      }

      @Override
      public VectorIJK differentiate(double t, VectorIJK buffer) {
        return buffer.setTo(function.getState(t, new StateVector()).getVelocity());
      }
    };
  }

  /**
   * Adapts the supplied state vector function to the {@link VectorIJKFunction} interface for the
   * velocity component of the state.
   * 
   * @param function the function to adapt
   * 
   * @return an adapter that passes the time argument through to the
   *         {@link StateVectorFunction#getState(double, StateVector)} to capture the velocity
   */
  public static VectorIJKFunction adaptVelocity(final StateVectorFunction function) {
    return new VectorIJKFunction() {

      @Override
      public VectorIJK evaluate(double t, VectorIJK buffer) {
        return buffer.setTo(function.getState(t, new StateVector()).getVelocity());
      }
    };
  }

  /**
   * Creates a new function from the source by applying a scale factor to the position and velocity
   * components of the evaluated state.
   * 
   * @param function the function to scale
   * @param scale the strictly positive scale factor to apply
   * 
   * @return a newly created function that wraps the supplied function and scales the output on
   *         evaluation
   * 
   * @throws IllegalArgumentException if the supplied scale factor is not strictly positive
   */
  public static StateVectorFunction scale(final StateVectorFunction function, final double scale) {
    return scale(function, scale, scale);
  }

  /**
   * Creates a new function from the source by applying independent scale factors to the position
   * and velocity components of the evaluated state.
   * 
   * @param function the function to scale
   * @param positionScale the strictly positive scale factor for the position
   * @param velocityScale the strictly positive scale factor for the velocity
   * 
   * @return a newly created function that wraps the supplied function and scales the output on
   *         evaluation
   * 
   * @throws IllegalArgumentException if either of the two supplied scale factors are not strictly
   *         positive
   */
  public static StateVectorFunction scale(final StateVectorFunction function,
      final double positionScale, final double velocityScale) {

    /*
     * Check to see if the scale argument is less than zero, if it is throw an illegal argument
     * exception as this method is only designed to apply positive scales to the function.
     */
    if (positionScale <= 0.0) {
      throw new IllegalArgumentException("Position scale factor: " + positionScale
          + " is invalid, as it is less than or equal to 0.0");
    }

    if (velocityScale <= 0.0) {
      throw new IllegalArgumentException("Velocity scale factor: " + velocityScale
          + " is invalid, as it is less than or equal to 0.0");
    }

    return new AbstractStateVectorFunctionWrapper(function) {

      @Override
      public VectorIJK getPosition(double time, VectorIJK buffer) {
        buffer = buffer == null ? new VectorIJK() : buffer;
        function.getPosition(time, buffer);
        return buffer.scale(positionScale);
      }

      @Override
      public StateVector getState(double time, StateVector buffer) {
        buffer = buffer == null ? new StateVector() : buffer;
        function.getState(time, buffer);
        buffer.getPosition().scale(positionScale);
        buffer.getVelocity().scale(velocityScale);
        return buffer;
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
  public static StateVectorFunction negate(final StateVectorFunction function) {
    return new AbstractReversedStateVectorFunctionWrapper(function) {

      @Override
      public VectorIJK getPosition(double time, VectorIJK buffer) {
        buffer = buffer == null ? new VectorIJK() : buffer;
        return function.getPosition(time, buffer).negate();
      }

      @Override
      public StateVector getState(double time, StateVector buffer) {
        buffer = buffer == null ? new StateVector() : buffer;
        return function.getState(time, buffer).negate();
      }
    };
  }

  /**
   * Creates a new function from the source by applying a state transformation.
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
  public static StateVectorFunction transform(final StateTransformFunction transform,
      final StateVectorFunction function) {

    /*
     * Check to see that the transform is appropriate for the function provided. All that this
     * implies is that the function is specified in the frame of the fromID from the transform.
     */
    if (!transform.getFromID().equals(function.getFrameID())) {
      throw new IllegalArgumentException("The frame the state vector function is specified in: "
          + function.getFrameID() + " is not equivalent to the fromID of the state transform: "
          + transform.getFromID());
    }

    return new AbstractStateVectorFunctionWrapper(function) {

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
        RotationMatrixIJK rotation = transform.getTransform(time, new RotationMatrixIJK());
        return rotation.mxv(buffer, buffer);
      }

      @Override
      public StateVector getState(double time, StateVector buffer) {
        buffer = buffer == null ? new StateVector() : buffer;
        function.getState(time, buffer);
        StateTransform stateTransform = transform.getStateTransform(time, new StateTransform());
        return stateTransform.mxv(buffer, buffer);
      }
    };

  }

  /**
   * Creates a fixed offset {@link StateVectorFunction} valid for all time.
   * <p>
   * As the vector is fixed, the velocity components will always evaluate to {@link VectorIJK#ZERO}.
   * </p>
   * 
   * @param targetID the target ID of the head of the fixed vector
   * 
   * @param observerID the observed ID of the tail of the fixed vector
   * 
   * @param frameID the frame ID of the fixed vector
   * 
   * @param vector the vector
   * 
   * @return a newly created {@link StateVectorFunction} that always evaluates to a fixed vector at
   *         any time with a zero velocity. Note the adapter copies the supplied vector, so it is
   *         not a view.
   */
  public static StateVectorFunction createFixed(final EphemerisID targetID,
      final EphemerisID observerID, final FrameID frameID, final UnwritableVectorIJK vector) {
    return createFixed(targetID, observerID, frameID, Coverage.ALL_TIME, vector);
  }

  /**
   * Creates a fixed offset {@link StateVectorFunction} valid for the specified coverage.
   * <p>
   * As the vector is fixed, the velocity components will always evaluate to {@link VectorIJK#ZERO}.
   * </p>
   * <p>
   * The resultant function will generate {@link EphemerisEvaluationException} if the supplied time
   * is not contained within the supplied coverage.
   * </p>
   * 
   * @param targetID the target ID of the head of the fixed vector
   * @param observerID the observer ID of the tail of the fixed vector
   * @param frameID the frame ID of the frame in which vector is expressed
   * @param coverage the coverage where this function is valid
   * @param vector the vector
   * 
   * @return a newly created {@link StateVectorFunction} that always evaluates to the fixed vector
   *         with zero velocity at times within the supplied coverage. Note the adapter copies the
   *         supplied vector, so it is not a view.
   */
  public static StateVectorFunction createFixed(final EphemerisID targetID,
      final EphemerisID observerID, final FrameID frameID, final Coverage coverage,
      final UnwritableVectorIJK vector) {
    return new StateVectorFunction() {

      private final UnwritableStateVector fixed =
          new UnwritableStateVector(new UnwritableVectorIJK(vector), VectorIJK.ZERO);

      @Override
      public EphemerisID getTargetID() {
        return targetID;
      }

      @Override
      public VectorIJK getPosition(double time, VectorIJK buffer) {
        buffer = buffer == null ? new VectorIJK() : buffer;
        if (!coverage.contains(time)) {
          throw new EphemerisEvaluationException("Unable to evaluate ephemeris at time: " + time);
        }
        return buffer.setTo(fixed.getPosition());
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

      @Override
      public StateVector getState(double time, StateVector buffer) {
        buffer = buffer == null ? new StateVector() : buffer;
        if (!coverage.contains(time)) {
          throw new EphemerisEvaluationException("Unable to evaluate ephemeris at time: " + time);
        }
        return buffer.setTo(fixed);
      }
    };
  }

  /**
   * Creates a {@link StateVectorFunction} that quadratically approximates the derivative of the
   * supplied function.
   *
   * @param function the function to differentiate
   * 
   * @return a function that places {@link StateVector#getVelocity()} in
   *         {@link StateVector#getPosition()} and the numerically derived acceleration in
   *         {@link StateVector#getVelocity()}
   */
  public static StateVectorFunction createQuadraticDerivative(StateVectorFunction function) {
    return new AbstractStateVectorFunctionWrapper(function) {

      /**
       * This parameter is determined based off the SPICE include file zzdyn.inc. It is selected
       * based on heuristics determined by NAIF. It is believed to be a decent option that is easily
       * computable.
       * 
       * The -27 below is the largest exponent where: 1.0 + 2**QEXP = 1.0 in IEEE 64bit floating
       * point arithmetic.
       * 
       * For time values that exceed 2**27, the value of delta should be time * 2**-27, for smaller
       * values it should simply be set to 1.
       */
      private final double MULTIPLIER = PicanteMath.pow(2.0, -27);

      @Override
      public StateVector getState(double time, StateVector buffer) {

        double delta = PicanteMath.max(1.0, MULTIPLIER * time);

        StateVector tMinus = new StateVector();
        function.getState(time - delta, tMinus);
        function.getState(time + delta, buffer);

        /*
         * Estimate the acceleration using the appropriate linear combination of the above velocity
         * values. Store the results in buffer.
         */
        VectorIJK.combine(0.5 / delta, buffer.getVelocity(), -0.5 / delta, tMinus.getVelocity(),
            buffer.getVelocity());

        /*
         * Now query the function at the requested time, and copy the velocity components into
         * buffer's position.
         */
        function.getState(time, tMinus);
        buffer.setPosition(tMinus.getVelocity());

        return buffer;
      }

      @Override
      public VectorIJK getPosition(double time, VectorIJK buffer) {
        StateVector vector = new StateVector();
        function.getState(time, vector);
        return buffer.setTo(vector.getVelocity());
      }

    };
  }

}
