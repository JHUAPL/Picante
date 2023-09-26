package picante.mechanics.providers.aberrated;

import static com.google.common.base.Preconditions.checkArgument;
import static com.google.common.base.Preconditions.checkNotNull;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.collect.ImmutableMap;
import picante.exceptions.BugException;
import picante.math.functions.DifferentiableUnivariateFunction;
import picante.math.functions.DifferentiableVectorIJKFunction;
import picante.math.functions.DifferentiableVectorIJKFunctions;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisAndFrameProvider;
import picante.mechanics.EphemerisID;
import picante.mechanics.EphemerisProvider;
import picante.mechanics.EphemerisSourceIOException;
import picante.mechanics.EphemerisSourceLinkException;
import picante.mechanics.FrameID;
import picante.mechanics.FrameProvider;
import picante.mechanics.FrameSourceIOException;
import picante.mechanics.FrameSourceLinkException;
import picante.mechanics.FrameTransformFunction;
import picante.mechanics.PositionVectorFunction;
import picante.mechanics.PositionVectorFunctions;
import picante.mechanics.StateTransformFunction;
import picante.mechanics.StateVectorFunction;
import picante.mechanics.StateVectorFunctions;

/**
 * An extension of the {@link EphemerisAndFrameProvider} that provides additional methods for
 * obtaining aberration corrected positions and states in inertial and non-inertial frames.
 * <p>
 * Due to the way the crucible ephemeris system supports &quot;position-only&quot; ephemerides,
 * there is an inherent disagreement between the position vector functions provided by this API and
 * the SPICE way of performing these computations. In order to properly compute the stellar
 * aberration corrected position of one object relative to another, the velocity of the observer
 * relative to an inertial reference is required. This implementation numerically differentiates the
 * position. However, if you request the state vector function, the position retrieval method will
 * utilize the velocity available from the supporting data.
 * </p>
 */
class NewtonianAberratedEphemerisProvider
    implements EphemerisAndFrameProvider, AberratedEphemerisProvider {

  private final EphemerisProvider ephemerisDelegate;
  private final FrameProvider frameDelegate;

  private final EphemerisID inertialPoint;
  private final FrameID inertialFrame;

  private final ImmutableMap<FrameID, EphemerisID> frameCenterMap;

  private final int numberOfIterations;
  private final double derivativeDeltaT;

  NewtonianAberratedEphemerisProvider(EphemerisProvider ephemerisDelegate,
      FrameProvider frameDelegate, EphemerisID inertialPoint, FrameID inertialFrame,
      Map<? extends FrameID, ? extends EphemerisID> frameCenterMap, int numberOfIterations,
      double derivativeDeltaT) {
    super();
    this.ephemerisDelegate = checkNotNull(ephemerisDelegate);
    this.frameDelegate = checkNotNull(frameDelegate);
    this.inertialPoint = checkNotNull(inertialPoint);
    this.inertialFrame = checkNotNull(inertialFrame);
    this.frameCenterMap = ImmutableMap.copyOf(frameCenterMap);
    checkArgument(numberOfIterations > 0);
    this.numberOfIterations = numberOfIterations;
    checkArgument(derivativeDeltaT > 0.0);
    this.derivativeDeltaT = derivativeDeltaT;
  }

  @Override
  public EphemerisID getInertialReferenceEphemerisID() {
    return inertialPoint;
  }

  @Override
  public FrameID getInertialReferenceFrameID() {
    return inertialFrame;
  }

  @Override
  public Map<FrameID, EphemerisID> getFrameCenterMap() {
    return frameCenterMap;
  }

  @Override
  public List<PositionVectorFunction> getEphemerisSourcesInLoadOrder() {
    return ephemerisDelegate.getEphemerisSourcesInLoadOrder();
  }

  @Override
  public Set<EphemerisID> getKnownObjects(Set<EphemerisID> buffer) {
    return ephemerisDelegate.getKnownObjects(buffer);
  }

  @Override
  public boolean isAwareOf(EphemerisID id) {
    return ephemerisDelegate.isAwareOf(id);
  }

  @Override
  public FrameProvider getFrameProvider() {
    return frameDelegate;
  }

  @Override
  public StateVectorFunction createStateVectorFunction(EphemerisID target, EphemerisID observer,
      FrameID frame, Coverage domain)
      throws EphemerisSourceIOException, EphemerisSourceLinkException {
    return ephemerisDelegate.createStateVectorFunction(target, observer, frame, domain);
  }

  @Override
  public PositionVectorFunction createPositionVectorFunction(EphemerisID target,
      EphemerisID observer, FrameID frame, Coverage domain)
      throws EphemerisSourceIOException, EphemerisSourceLinkException {
    return ephemerisDelegate.createPositionVectorFunction(target, observer, frame, domain);
  }

  @Override
  public List<FrameTransformFunction> getFrameSourcesInLoadOrder() {
    return frameDelegate.getFrameSourcesInLoadOrder();
  }

  @Override
  public Set<FrameID> getKnownFrames(Set<FrameID> buffer) {
    return frameDelegate.getKnownFrames(buffer);
  }

  @Override
  public boolean isAwareOf(FrameID id) {
    return frameDelegate.isAwareOf(id);
  }

  @Override
  public FrameTransformFunction createFrameTransformFunction(FrameID fromID, FrameID toID,
      Coverage domain) throws FrameSourceIOException, FrameSourceLinkException {
    return frameDelegate.createFrameTransformFunction(fromID, toID, domain);
  }

  @Override
  public StateTransformFunction createStateTransformFunction(FrameID fromID, FrameID toID,
      Coverage domain) throws FrameSourceIOException, FrameSourceLinkException {
    return frameDelegate.createStateTransformFunction(fromID, toID, domain);
  }

  private InertialLightTimePositionVectorFunction createInertialLightTimePositionFunction(
      EphemerisID target, EphemerisID observer, FrameID frame, Coverage domain,
      AberrationCorrection correction)
      throws EphemerisSourceIOException, EphemerisSourceLinkException {
    return new InertialLightTimePositionVectorFunction(
        createPositionVectorFunction(observer, inertialPoint, frame, domain),
        createPositionVectorFunction(target, inertialPoint, frame, domain), correction,
        numberOfIterations);

  }

  private AberratedPositionVectorFunction createInertialPositionFunction(EphemerisID target,
      EphemerisID observer, FrameID frame, Coverage domain, AberrationCorrection correction)
      throws EphemerisSourceIOException, EphemerisSourceLinkException {
    if (correction == AberrationCorrection.LT || correction == AberrationCorrection.XLT) {
      return createInertialLightTimePositionFunction(target, observer, frame, domain, correction);
    }
    if (correction == AberrationCorrection.LT_S || correction == AberrationCorrection.XLT_S) {

      InertialLightTimePositionVectorFunction ltFunction = createInertialLightTimePositionFunction(
          target, observer, frame, domain, AberrationCorrection.stripStellarAberration(correction));
      PositionVectorFunction obsFunction =
          createPositionVectorFunction(observer, inertialPoint, frame, domain);
      DifferentiableVectorIJKFunction observerPosition = DifferentiableVectorIJKFunctions
          .quadraticApproximation(PositionVectorFunctions.adapt(obsFunction), derivativeDeltaT);

      return new InertialStellarAberratedPositionVectorFunction(ltFunction, observerPosition,
          correction);
    }

    /*
     * Any other correction type results in a bug exception.
     */
    throw new BugException("Unhandled aberration correction type:" + correction);

  }


  @Override
  public AberratedPositionVectorFunction createAberratedPositionVectorFunction(EphemerisID target,
      EphemerisID observer, FrameID frame, Coverage domain, AberrationCorrection correction)
      throws EphemerisSourceIOException, EphemerisSourceLinkException {

    /*
     * Not much to do if the state is geometric.
     */
    if (correction == AberrationCorrection.NONE) {
      return new GeometricPositionVectorFunction(
          createPositionVectorFunction(target, observer, frame, domain));
    }

    /*
     * Check to see if the requested frame is inertial, if it is then we do not need to layer the
     * frame adjustment code.
     */
    if (frame.isInertial()) {
      return createInertialPositionFunction(target, observer, frame, domain, correction);
    }

    /*
     * If we reach here we have a requested aberration correction that must be applied in a
     * non-inertial frame. Assemble the pieces to construct the implementation that will perform
     * this correction. First verify that the frame center map contains the frame of interest.
     */
    if (!frameCenterMap.containsKey(frame)) {
      throw new EphemerisSourceLinkException("Unable to locate center for frame: " + frame
          + " It must be included in the entries supplied to "
          + "the frame center map at construction of this provider.");
    }

    AberratedPositionVectorFunction inertial =
        createInertialPositionFunction(target, observer, inertialFrame, domain, correction);

    AberrationCorrection ltCorrection =
        correction.useStellarAberration() ? AberrationCorrection.stripStellarAberration(correction)
            : correction;

    /*
     * Handle the case where the non-inertial frame is centered on the observer. We'll subversively
     * place an implementation of the observer to frame center correction function that returns 0
     * for all light times and 0 for all derivatives.
     */
    EphemerisID frameCenter = frameCenterMap.get(frame);
    AberratedPositionVectorFunction center;
    if (frameCenter.equals(observer)) {
      center = new EqualObserverTargetAberratedStateVectorFunction(frameCenter, observer,
          inertialFrame, domain, ltCorrection);
    } else {
      center = createInertialLightTimePositionFunction(frameCenter, observer, inertialFrame, domain,
          ltCorrection);
    }

    /*
     * Attempt to construct the frame transform from the inertial reference frame to the requested
     * frame.
     */
    try {
      FrameTransformFunction transform =
          frameDelegate.createFrameTransformFunction(inertialFrame, frame, domain);
      return new NonInertialAberratedPositionVectorFunction(inertial, center, transform);
    } catch (FrameSourceIOException e) {
      throw new EphemerisSourceIOException("Unable to instantiate ephemeris function due "
          + "to IO exception in the frame subsystem.", e);
    } catch (FrameSourceLinkException e) {
      throw new EphemerisSourceLinkException(
          "Unable to link ephemeris due to an inability " + "to link the required frames.", e);
    }

  }

  private InertialLightTimeStateVectorFunction createInertialLightTimeStateFunction(
      EphemerisID target, EphemerisID observer, FrameID frame, Coverage domain,
      AberrationCorrection correction)
      throws EphemerisSourceIOException, EphemerisSourceLinkException {
    return new InertialLightTimeStateVectorFunction(
        createStateVectorFunction(observer, inertialPoint, frame, domain),
        createStateVectorFunction(target, inertialPoint, frame, domain), correction,
        numberOfIterations);

  }

  private AberratedStateVectorFunction createInertialStateFunction(EphemerisID target,
      EphemerisID observer, FrameID frame, Coverage domain, AberrationCorrection correction)
      throws EphemerisSourceIOException, EphemerisSourceLinkException {
    if (correction == AberrationCorrection.LT || correction == AberrationCorrection.XLT) {
      return createInertialLightTimeStateFunction(target, observer, frame, domain, correction);
    }
    if (correction == AberrationCorrection.LT_S || correction == AberrationCorrection.XLT_S) {

      InertialLightTimeStateVectorFunction ltFunction = createInertialLightTimeStateFunction(target,
          observer, frame, domain, AberrationCorrection.stripStellarAberration(correction));
      StateVectorFunction obsFunction =
          createStateVectorFunction(observer, inertialPoint, frame, domain);
      DifferentiableVectorIJKFunction observerVelocity =
          DifferentiableVectorIJKFunctions.quadraticApproximation(
              StateVectorFunctions.adaptVelocity(obsFunction), derivativeDeltaT);

      return new InertialStellarAberratedStateVectorFunction(ltFunction, observerVelocity,
          correction, derivativeDeltaT);
    }

    /*
     * Any other correction type results in a bug exception.
     */
    throw new BugException("Unhandled aberration correction type:" + correction);

  }


  @Override
  public AberratedStateVectorFunction createAberratedStateVectorFunction(EphemerisID target,
      EphemerisID observer, FrameID frame, Coverage domain, AberrationCorrection correction)
      throws EphemerisSourceIOException, EphemerisSourceLinkException {

    /*
     * Not much to do if the state is geometric.
     */
    if (correction == AberrationCorrection.NONE) {
      return new GeometricStateVectorFunction(
          createStateVectorFunction(target, observer, frame, domain));
    }

    if (frame.isInertial()) {
      return createInertialStateFunction(target, observer, frame, domain, correction);
    }

    /*
     * If we reach here we have a requested aberration correction that must be applied in a
     * non-inertial frame. Assemble the pieces to construct the implementation that will perform
     * this correction. First verify that the frame center map contains the frame of interest.
     */
    if (!frameCenterMap.containsKey(frame)) {
      throw new EphemerisSourceLinkException("Unable to locate center for frame: " + frame
          + " It must be included in the entries supplied to "
          + "the frame center map at construction of this provider.");
    }

    AberratedStateVectorFunction inertial =
        createInertialStateFunction(target, observer, inertialFrame, domain, correction);

    AberrationCorrection ltCorrection =
        correction.useStellarAberration() ? AberrationCorrection.stripStellarAberration(correction)
            : correction;

    /*
     * Handle the case where the non-inertial frame is centered on the observer. We'll subversively
     * place an implementation of the observer to frame center correction function that returns 0
     * for all light times and 0 for all derivatives.
     */
    EphemerisID frameCenter = frameCenterMap.get(frame);
    AberratedStateVectorFunction center;
    if (frameCenter.equals(observer)) {
      center = new EqualObserverTargetAberratedStateVectorFunction(frameCenter, observer,
          inertialFrame, domain, ltCorrection);
    } else {
      center = createInertialLightTimeStateFunction(frameCenter, observer, inertialFrame, domain,
          ltCorrection);
    }

    /*
     * Attempt to construct the frame transform from the inertial reference frame to the requested
     * frame.
     */
    try {
      StateTransformFunction transform =
          frameDelegate.createStateTransformFunction(inertialFrame, frame, domain);
      return new NonInertialAberratedStateVectorFunction(inertial, center, transform);
    } catch (FrameSourceIOException e) {
      throw new EphemerisSourceIOException("Unable to instantiate ephemeris function due "
          + "to IO exception in the frame subsystem.", e);
    } catch (FrameSourceLinkException e) {
      throw new EphemerisSourceLinkException(
          "Unable to link ephemeris due to an inability " + "to link the required frames.", e);
    }

  }

  // TODO: Create univariate functions that do not have the derivative methods (as position only
  // vector functions could be provided as a source without derivatives.


  @Override
  public DifferentiableUnivariateFunction createLightTimeFunction(EphemerisID target,
      EphemerisID observer, Coverage domain, AberrationCorrection correction)
      throws EphemerisSourceIOException, EphemerisSourceLinkException {

    checkArgument(!correction.isGeometric(),
        "Geometric states can not be used to produce light times.");

    final AberratedStateVectorFunction function = createAberratedStateVectorFunction(target,
        observer, inertialFrame, domain, AberrationCorrection.stripStellarAberration(correction));

    return new DifferentiableUnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return function.getLightTime(t);
      }

      @Override
      public double differentiate(double t) {
        return function.getLightTimeDerivative(t);
      }
    };
  }


  @Override
  public DifferentiableUnivariateFunction createLightTimeAdjustingFunction(EphemerisID target,
      EphemerisID observer, Coverage domain, AberrationCorrection correction)
      throws EphemerisSourceIOException, EphemerisSourceLinkException {

    checkArgument(!correction.isGeometric(),
        "Geometric states can not be used to produce light times.");

    final AberratedStateVectorFunction function = createAberratedStateVectorFunction(target,
        observer, inertialFrame, domain, AberrationCorrection.stripStellarAberration(correction));

    /*
     * If the correction requested is a receipt based one, then simply multiple the light time and
     * its derivative by -1, since we want the time when photons leave the target.
     */
    final double receiptMultiplier = correction.isReceipt() ? -1.0 : 1.0;

    return new DifferentiableUnivariateFunction() {

      @Override
      public double evaluate(double t) {
        return t + receiptMultiplier * function.getLightTime(t);
      }

      @Override
      public double differentiate(double t) {
        return 1 + receiptMultiplier * function.getLightTimeDerivative(t);
      }
    };

  }
}
