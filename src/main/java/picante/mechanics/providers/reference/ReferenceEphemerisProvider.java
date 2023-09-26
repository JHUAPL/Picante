package picante.mechanics.providers.reference;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import com.google.common.collect.ImmutableList;
import picante.math.vectorspace.VectorIJK;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisAndFrameProvider;
import picante.mechanics.EphemerisID;
import picante.mechanics.EphemerisSourceLinkException;
import picante.mechanics.FrameID;
import picante.mechanics.FrameTransformFunction;
import picante.mechanics.PositionVectorFunction;
import picante.mechanics.StateTransformFunction;
import picante.mechanics.StateVector;
import picante.mechanics.StateVectorFunction;

/**
 * Provides a reference implementation of the ephemeris provider interface. This interface creates
 * position and state vector functions in a thread safe, but slightly inefficient manner. The
 * coverage intervals supplied to create the vector functions are ignored by this implementation.
 * Error generation is deferred to the evaluation of the function.
 * <p>
 * With regards to thread safety, instances of this provider may be used to create vector functions
 * from multiple threads safely. However, the individual vector functions created are not thread
 * safe. In general you need multiple copies of the same function to perform evaluation from
 * different threads.
 * </p>
 */
public class ReferenceEphemerisProvider implements EphemerisAndFrameProvider {

  /**
   * Creates an implementation of the {@link StateVectorFunction} interface that is utilized purely
   * as an entry in a list of <code>StateVectorFunction</code>s or
   * <code>PositionVectorFunction</code>s as a marker.
   * 
   * @return an implementation of the StateVectorFunction that throws exceptions whenever its
   *         methods are invoked.
   */
  private static StateVectorFunction createMarkerFunction() {
    return new StateVectorFunction() {

      private UnsupportedOperationException bugException = new UnsupportedOperationException(
          "Implementation error.  This method should never be invoked.");

      @Override
      public StateVector getState(@SuppressWarnings("unused") double time,
          @SuppressWarnings("unused") StateVector buffer) {
        throw bugException;
      }

      @Override
      public VectorIJK getPosition(@SuppressWarnings("unused") double time,
          @SuppressWarnings("unused") VectorIJK buffer) {
        throw bugException;
      }

      @Override
      public Coverage getCoverage() {
        throw bugException;
      }

      @Override
      public FrameID getFrameID() {
        throw bugException;
      }

      @Override
      public EphemerisID getObserverID() {
        throw bugException;
      }

      @Override
      public EphemerisID getTargetID() {
        throw bugException;
      }

    };
  }

  /**
   * An instance of the {@link CodeProvider} interface suitable for usage in
   * <code>ChainLinkEngine</code>s that link <code>PositionVectorFunction</code> s together.
   */
  private static final EphemerisCodeProvider<PositionVectorFunction> POSITION_PROVIDER =
      new EphemerisCodeProvider<PositionVectorFunction>();

  /**
   * An instance of the {@link CodeProvider} interface suitable for usage in
   * <code>ChainLinkEngine</code>s that link <code>StateVectorFunction</code>s together.
   */
  private static final EphemerisCodeProvider<StateVectorFunction> STATE_PROVIDER =
      new EphemerisCodeProvider<StateVectorFunction>();

  /**
   * Marker instance used to indicate the separation between the forward evaluation of a linked
   * chain and the backward evaluation. Functions that occur prior to this in a chained list of
   * functions can be chained directly by the function returned. Functions after, must be negated
   * prior to their combination.
   */
  static final StateVectorFunction SEPARATOR = createMarkerFunction();

  /**
   * Marker instance used to indicate the separation between the extent of the forward chain and the
   * extent of the backward chain in the event that a connection is unable to be located. Primarily
   * intended to create more useful error messages in the event of a broken link.
   */
  static final StateVectorFunction BROKEN_LINK = createMarkerFunction();

  /**
   * Instance of the chain link engine used to connect any state vector or position vector functions
   * provided to the constructor of this class.
   */
  private final ChainLinkEngine<EphemerisID, PositionVectorFunction, EphemerisCodeProvider<PositionVectorFunction>> positionEngine;

  /**
   * Instance of the chain link engine used to connect only state vector functions provided to the
   * constructor of this class.
   */
  private final ChainLinkEngine<EphemerisID, StateVectorFunction, EphemerisCodeProvider<StateVectorFunction>> stateEngine;

  /**
   * The list of sources loaded into the application from lowest priority to highest priority.
   */
  private final ImmutableList<PositionVectorFunction> sources;

  /**
   * The set of ephemeris ID codes known to the instance.
   */
  private final Set<EphemerisID> knownObjects;

  /**
   * The frame provider used to support any frame or state transformation necessary in the
   * evaluation of the state or position vector functions created by this provider.
   */
  private final ReferenceFrameProvider provider;

  /**
   * Constructs an instance of the reference ephemeris provider from the supplied list of sources.
   * 
   * @param sources a list of sources, in order from lowest priority (low index) to highest priority
   *        (high index).
   * @param frameSources a list of frame sources, in order from lowest priority (low index) to
   *        highest priority (high index), that will be used internally to construct a
   *        <code>ReferenceFrameProvider</code> to support the state and position vector chaining
   *        performed by functions created by this provider
   * 
   */
  public ReferenceEphemerisProvider(List<? extends PositionVectorFunction> sources,
      List<? extends FrameTransformFunction> frameSources) {

    /*
     * Capture the list of sources in load priority order.
     */
    this.sources = ImmutableList.copyOf(sources);
    this.knownObjects = new HashSet<EphemerisID>();

    this.provider = new ReferenceFrameProvider(frameSources);

    /*
     * Create separate lists to receive state and position vector functions.
     */
    List<PositionVectorFunction> positionFunctions = new LinkedList<PositionVectorFunction>();
    List<StateVectorFunction> stateFunctions = new LinkedList<StateVectorFunction>();

    /*
     * Loop over all of the supplied sources. Place all position and state vector functions into the
     * position vector function list. Place all the state vector functions into the state vector
     * functions list. Add all the visited ephemeris ID codes to the set of known objects.
     */
    for (PositionVectorFunction function : this.sources) {
      positionFunctions.add(function);
      knownObjects.add(function.getObserverID());
      knownObjects.add(function.getTargetID());
      if (function instanceof StateVectorFunction) {
        stateFunctions.add((StateVectorFunction) function);
      }
    }


    this.positionEngine =
        new ChainLinkEngine<EphemerisID, PositionVectorFunction, EphemerisCodeProvider<PositionVectorFunction>>(
            positionFunctions, POSITION_PROVIDER, SEPARATOR, BROKEN_LINK);

    this.stateEngine =
        new ChainLinkEngine<EphemerisID, StateVectorFunction, EphemerisCodeProvider<StateVectorFunction>>(
            stateFunctions, STATE_PROVIDER, SEPARATOR, BROKEN_LINK);

  }

  /**
   * Simple method that consolidates the handling of exceptional cases that arise when creating
   * state or position vector functions.
   * 
   * @param target the ephemeris ID code of the target body
   * @param observer the ephemeris ID code of the observing body
   * @param frame the frame ID code of the output frame
   * 
   * @return either null, in the event no exceptional cases have occurred or an identity state
   *         vector function connecting observer to target in the event they are equivalent ID codes
   * 
   */
  private StateVectorFunction handleExceptionalCases(EphemerisID target, EphemerisID observer,
      FrameID frame) {

    if (!knownObjects.contains(target)) {
      throw new EphemerisSourceLinkException(
          "Provider does not contain any ephemeris data for object: " + target.getName());
    }

    if (!knownObjects.contains(observer)) {
      throw new EphemerisSourceLinkException(
          "Provider does not contain any ephemeris data for object: " + observer.getName());
    }

    if (!provider.isAwareOf(frame)) {
      throw new EphemerisSourceLinkException(
          "Embedded frame provider does not contain data for frame: " + frame.getName());
    }

    if (target.equals(observer)) {
      return new IdentityStateVectorFunction(target, observer, frame);
    }

    return null;

  }

  /**
   * {@inheritDoc}
   * 
   * The function created is not safe for accessing from multiple threads. Create multiple copies if
   * this is required.
   */
  @Override
  public PositionVectorFunction createPositionVectorFunction(EphemerisID target,
      EphemerisID observer, FrameID frame, @SuppressWarnings("unused") Coverage domain) {

    PositionVectorFunction result = handleExceptionalCases(target, observer, frame);

    if (result != null) {
      return result;
    }

    return new ReferencePositionVectorFunction<PositionVectorFunction>(target, observer, frame,
        positionEngine, provider);

  }

  /**
   * {@inheritDoc}
   * 
   * The function created is not safe for accessing from multiple threads. Create multiple copies if
   * this is required.
   */
  @Override
  public StateVectorFunction createStateVectorFunction(EphemerisID target, EphemerisID observer,
      FrameID frame, @SuppressWarnings("unused") Coverage domain) {

    StateVectorFunction result = handleExceptionalCases(target, observer, frame);

    if (result != null) {
      return result;
    }

    return new ReferenceStateVectorFunction(target, observer, frame, stateEngine, provider);
  }

  @Override
  public ReferenceFrameProvider getFrameProvider() {
    return provider;
  }

  @Override
  public Set<EphemerisID> getKnownObjects(Set<EphemerisID> buffer) {
    buffer.addAll(knownObjects);
    return buffer;
  }

  @Override
  public boolean isAwareOf(EphemerisID id) {
    return knownObjects.contains(id);
  }

  @Override
  public List<PositionVectorFunction> getEphemerisSourcesInLoadOrder() {
    return sources;
  }

  @Override
  public List<FrameTransformFunction> getFrameSourcesInLoadOrder() {
    return provider.getFrameSourcesInLoadOrder();
  }

  @Override
  public Set<FrameID> getKnownFrames(Set<FrameID> buffer) {
    return provider.getKnownFrames(buffer);
  }

  @Override
  public boolean isAwareOf(FrameID id) {
    return provider.isAwareOf(id);
  }

  @Override
  public FrameTransformFunction createFrameTransformFunction(FrameID fromID, FrameID toID,
      Coverage domain) {
    return provider.createFrameTransformFunction(fromID, toID, domain);
  }

  @Override
  public StateTransformFunction createStateTransformFunction(FrameID fromID, FrameID toID,
      Coverage domain) {
    return provider.createStateTransformFunction(fromID, toID, domain);
  }

}


/**
 * Implementation of the state vector function that simply supplies a zero body state vector for all
 * time. It is designed to supply callers that request functions that connect equal ephemeris ID
 * codes.
 */
class IdentityStateVectorFunction implements StateVectorFunction {

  private final EphemerisID target;
  private final EphemerisID observer;
  private final FrameID frame;

  /**
   * Construct an identity state vector function that connects two, equal ephemeris ID codes.
   * 
   * @param target an ephemeris ID code
   * @param observer another ephemeris ID code, equivalent (through equals()) to targetID
   * @param frame the frame ID code
   */
  public IdentityStateVectorFunction(EphemerisID target, EphemerisID observer, FrameID frame) {
    super();
    this.target = target;
    this.observer = observer;
    this.frame = frame;
  }

  @Override
  public StateVector getState(@SuppressWarnings("unused") double time, StateVector buffer) {
    buffer.clear();
    return buffer;
  }

  @Override
  public VectorIJK getPosition(@SuppressWarnings("unused") double time, VectorIJK buffer) {
    buffer.setTo(VectorIJK.ZERO);
    return buffer;
  }

  @Override
  public Coverage getCoverage() {
    return Coverage.ALL_TIME;
  }

  @Override
  public FrameID getFrameID() {
    return frame;
  }

  @Override
  public EphemerisID getObserverID() {
    return observer;
  }

  @Override
  public EphemerisID getTargetID() {
    return target;
  }

}


/**
 * Implementation of the {@link CodeProvider} interface required by the chain link engine utilized
 * internally.
 * 
 * @param <B> a function that extends PositionVectorFunction
 */
class EphemerisCodeProvider<B extends PositionVectorFunction>
    implements CodeProvider<EphemerisID, B> {

  @Override
  public EphemerisID getLeafCode(B function) {
    return function.getTargetID();
  }

  @Override
  public EphemerisID getNodeCode(B function) {
    return function.getObserverID();
  }

  @Override
  public boolean validAt(B function, double time) {
    return function.getCoverage().contains(time);
  }

}
