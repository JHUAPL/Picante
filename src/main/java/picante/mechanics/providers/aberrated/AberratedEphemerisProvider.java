package picante.mechanics.providers.aberrated;

import java.util.Map;
import picante.math.functions.DifferentiableUnivariateFunction;
import picante.mechanics.CelestialBodies;
import picante.mechanics.CelestialFrames;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisAndFrameProvider;
import picante.mechanics.EphemerisID;
import picante.mechanics.EphemerisProvider;
import picante.mechanics.EphemerisSourceIOException;
import picante.mechanics.EphemerisSourceLinkException;
import picante.mechanics.FrameID;
import picante.mechanics.FrameProvider;

/**
 * Extension of {@link EphemerisAndFrameProvider} interface that provides additional methods for
 * computing aberration corrected states and positions.
 */
public interface AberratedEphemerisProvider extends EphemerisAndFrameProvider {

  /**
   * Creates an instance of a Newtonian aberration corrected ephemeris provider
   * 
   * @param ephemerisProvider the ephemeris provider to which geometric requests for positions and
   *        states are delegated
   * @param frameProvider the frame provider to which transform requests are delegated
   * @param frameCenterMap the map of frame IDs to ephemeris IDs which define the frame centers for
   *        the purpose of light time correction
   * 
   * @return a provider that uses a single iteraton of the light time contraction map to estimate
   *         light times and the default inertial frame {@link CelestialFrames#J2000} centered on
   *         the quasi-inertial reference point {@link CelestialBodies#SOLAR_SYSTEM_BARYCENTER}
   */
  public static AberratedEphemerisProvider createSingleIteration(
      EphemerisProvider ephemerisProvider, FrameProvider frameProvider,
      Map<? extends FrameID, ? extends EphemerisID> frameCenterMap) {
    return new NewtonianAberratedEphemerisProvider(ephemerisProvider, frameProvider,
        CelestialBodies.SOLAR_SYSTEM_BARYCENTER, CelestialFrames.J2000, frameCenterMap, 1, 1.0);
  }

  public static AberratedEphemerisProvider createSingleIteration(EphemerisAndFrameProvider provider,
      Map<? extends FrameID, ? extends EphemerisID> frameCenterMap) {
    return createSingleIteration(provider, provider, frameCenterMap);
  }

  /**
   * Creates an instance of a Newtonian aberration corrected ephemeris provider
   * 
   * @param ephemerisProvider the ephemeris provider to which geometric requests for positions and
   *        states are delegated
   * @param frameProvider the frame provider to which transform requests are delegated
   * @param frameCenterMap the map of frame IDs to ephemeris IDs which define the frame centers for
   *        the purpose of light time correction
   * 
   * @return a provider that uses a single iteraton of the light time contraction map to estimate
   *         light times and the default inertial frame {@link CelestialFrames#J2000} centered on
   *         the quasi-inertial reference point {@link CelestialBodies#SOLAR_SYSTEM_BARYCENTER}
   */
  public static AberratedEphemerisProvider createTripleIteration(
      EphemerisProvider ephemerisProvider, FrameProvider frameProvider,
      Map<? extends FrameID, ? extends EphemerisID> frameCenterMap) {
    return new NewtonianAberratedEphemerisProvider(ephemerisProvider, frameProvider,
        CelestialBodies.SOLAR_SYSTEM_BARYCENTER, CelestialFrames.J2000, frameCenterMap, 3, 1.0);
  }

  public static AberratedEphemerisProvider createTripleIteration(EphemerisAndFrameProvider provider,
      Map<? extends FrameID, ? extends EphemerisID> frameCenterMap) {
    return createTripleIteration(provider, provider, frameCenterMap);
  }


  /**
   * Returns the {@link EphemerisID} for the inertial reference point used in light time and stellar
   * aberration corrections.
   * 
   * @return the ephemerisID
   */
  EphemerisID getInertialReferenceEphemerisID();

  /**
   * Returns the {@link FrameID} for the inertial reference frame used in light time and stellar
   * aberration corrections.
   * 
   * @return the frameID
   */
  FrameID getInertialReferenceFrameID();

  /**
   * Retrieves the map of frame IDs to their corresponding &quot;center&quot; ephemeris IDs used in
   * light time corrections through the frame.
   * 
   * @return an unmodifiable view of the internal map held by the instance
   */
  Map<FrameID, EphemerisID> getFrameCenterMap();

  /**
   * Creates a position vector function with the requested aberration corrections applied.
   * <p>
   * Note: if a function is requested with {@link AberrationCorrection#NONE} the method
   * {@link AberratedPositionVectorFunction#getLightTime(double)} will always throw a
   * {@link UnsupportedOperationException}, since it is impossible to provide light time without
   * knowing the direction in which light is traveling.
   * </p>
   * 
   * @param target the target of interest
   * @param observer the observer, at which all supplied times are evaluated
   * @param frame the reference frame
   * @param domain the time domain over which the function is to be queried.
   * @param correction the correction to apply to the implementation
   * 
   * @return a newly created function that applies the requested aberration corrections
   * 
   * @throws EphemerisSourceIOException if an I/O error occurs in constructing the function
   * @throws EphemerisSourceLinkException if a data source is missing or otherwise unable to satisfy
   *         the request
   */
  AberratedPositionVectorFunction createAberratedPositionVectorFunction(EphemerisID target,
      EphemerisID observer, FrameID frame, Coverage domain, AberrationCorrection correction)
      throws EphemerisSourceIOException, EphemerisSourceLinkException;

  /**
   * Creates a state vector function with the requested aberration corrections applied.
   * <p>
   * Note: if a function is requested with {@link AberrationCorrection#NONE} the methods
   * {@link AberratedStateVectorFunction#getLightTime(double)}
   * {@link AberratedStateVectorFunction#getLightTimeDerivative(double)} will always throw a
   * {@link UnsupportedOperationException}, since it is impossible to provide light time without
   * knowing the direction in which light is traveling.
   * </p>
   * 
   * @param target the target of interest
   * @param observer the observer, at which all supplied times are evaluated
   * @param frame the reference frame
   * @param domain the time domain over which the function is to be queried.
   * @param correction the correction to apply to the implementation
   * 
   * @return a newly created function that applies the requested aberration corrections
   * 
   * @throws EphemerisSourceIOException if an I/O error occurs in constructing the function
   * @throws EphemerisSourceLinkException if a data source is missing or otherwise unable to satisfy
   *         the request
   */
  AberratedStateVectorFunction createAberratedStateVectorFunction(EphemerisID target,
      EphemerisID observer, FrameID frame, Coverage domain, AberrationCorrection correction)
      throws EphemerisSourceIOException, EphemerisSourceLinkException;

  /**
   * Convenience method that creates a {@link DifferentiableUnivariateFunction} that provides the
   * light time between the observer and the target, where the time is specified at the observer.
   * 
   * @param target
   * @param observer
   * @param frame
   * @param domain
   * @param correction
   * @return
   * @throws EphemerisSourceIOException
   * @throws EphemerisSourceLinkException
   */
  DifferentiableUnivariateFunction createLightTimeFunction(EphemerisID target, EphemerisID observer,
      Coverage domain, AberrationCorrection correction)
      throws EphemerisSourceIOException, EphemerisSourceLinkException;

  /**
   * Convenience method that creates a {@link DifferentiableUnivariateFunction} that provides the
   * time at which light leaves {@link AberrationCorrection#isReceipt()} or arrives
   * {@link AberrationCorrection#isTransmission()} at a target.
   * 
   * @param target
   * @param observer
   * @param frame
   * @param domain
   * @param correction
   * @return
   * @throws EphemerisSourceIOException
   * @throws EphemerisSourceLinkException
   */
  DifferentiableUnivariateFunction createLightTimeAdjustingFunction(EphemerisID target,
      EphemerisID observer, Coverage domain, AberrationCorrection correction)
      throws EphemerisSourceIOException, EphemerisSourceLinkException;

}
