package picante.mechanics.providers.reference;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import com.google.common.collect.ImmutableList;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;
import picante.mechanics.FrameID;
import picante.mechanics.FrameProvider;
import picante.mechanics.FrameSourceLinkException;
import picante.mechanics.FrameTransformFunction;
import picante.mechanics.StateTransform;
import picante.mechanics.StateTransformFunction;

/**
 * Provides a reference implementation of the frame provider interface. This interface creates frame
 * and state transform functions in a thread safe, but slightly inefficient manner. The coverage
 * intervals supplied to the create transform functions are ignored by this implementation. Error
 * generation is deferred to evaluation of the transform.
 * <p>
 * With regards to thread safety, instances of this provider may be used to create transform
 * functions from multiple threads safely. However, the individual transform functions created are
 * not thread safe. In general you need multiple copies of the same transform to evaluate from
 * different threads.
 * </p>
 */
public class ReferenceFrameProvider implements FrameProvider {

  /**
   * Creates an implementation of the {@link StateTransformFunction} interface that is utilized
   * purely as an entry in a list of <code>StateTransformFunction</code>s or
   * <code>FrameTransformFunction</code>s as a marker.
   * 
   * @return an implementation of the StateTransformFunction that throws exceptions whenever its
   *         methods are invoked
   */
  private static StateTransformFunction createMarkerFunction() {
    return new StateTransformFunction() {

      private UnsupportedOperationException bugException = new UnsupportedOperationException(
          "Implementation error.  This method should never be invoked.");

      @Override
      public StateTransform getStateTransform(@SuppressWarnings("unused") double time,
          @SuppressWarnings("unused") StateTransform buffer) {
        throw bugException;
      }

      @Override
      public Coverage getCoverage() {
        throw bugException;
      }

      @Override
      public FrameID getFromID() {
        throw bugException;
      }

      @Override
      public FrameID getToID() {
        throw bugException;
      }

      @Override
      public RotationMatrixIJK getTransform(@SuppressWarnings("unused") double time,
          @SuppressWarnings("unused") RotationMatrixIJK buffer) {
        throw bugException;
      }

    };
  }

  /**
   * An instance of the {@link CodeProvider} interface suitable for usage in
   * <code>ChainLinkEngine</code>s that link <code>FrameTransformFunctions</code> together.
   */
  private final static FrameCodeProvider<FrameTransformFunction> FRAME_PROVIDER =
      new FrameCodeProvider<FrameTransformFunction>();

  /**
   * An instance of the {@link CodeProvider} interface suitable for usage in
   * <code>ChainLinkEngine</code>s that link <code>StateTransformFunctions</code> together.
   */
  private final static FrameCodeProvider<StateTransformFunction> STATE_PROVIDER =
      new FrameCodeProvider<StateTransformFunction>();

  /**
   * Marker instance used to indicate the separation between the forward evaluation of a linked
   * chain and the backward evaluation. Functions that occur prior to this in a chained list of
   * functions can be chained directly by the transform returned. Functions after, must be inverted
   * prior to their combination.
   */
  final static StateTransformFunction SEPARATOR = createMarkerFunction();

  /**
   * Marker instance used to indicate the separation between the extent of the forward chain and the
   * extent of the backward chain in the event that a connection is unable to be located. Primarly
   * intended to create more useful error messages in the event of a broken link.
   */
  final static StateTransformFunction BROKEN_LINK = createMarkerFunction();

  /**
   * Instance of the chain link engine used to connect any frame or state transform functions
   * provided to the constructor of this class.
   */
  private final ChainLinkEngine<FrameID, FrameTransformFunction, FrameCodeProvider<FrameTransformFunction>> frameEngine;

  /**
   * Instance of the chain link engine used to connect any state transform functions provided to the
   * constructor of this class.
   */
  private final ChainLinkEngine<FrameID, StateTransformFunction, FrameCodeProvider<StateTransformFunction>> stateEngine;

  /**
   * The list of sources loaded into the application from lowest priority to highest priority.
   */
  private final ImmutableList<FrameTransformFunction> sources;

  /**
   * The set of frame ID codes known to the instance.
   */
  private final Set<FrameID> knownFrames;

  /**
   * Constructs an instance of the reference frame provider from the supplied list of sources.
   * 
   * @param sources a list of sources, in order from lowest priority (low index) to highest priority
   *        (high index).
   * 
   */
  public ReferenceFrameProvider(List<? extends FrameTransformFunction> sources) {


    /*
     * Capture the list of sources in load priority order.
     */

    this.sources = ImmutableList.copyOf(sources);
    this.knownFrames = new HashSet<FrameID>();

    /*
     * Create separate lists to receive frame transform and state transform functions.
     */
    List<FrameTransformFunction> frameFunctions = new LinkedList<FrameTransformFunction>();
    List<StateTransformFunction> stateFunctions = new LinkedList<StateTransformFunction>();

    /*
     * Loop over all supplied sources. Place all state and frame transform functions into the frame
     * transform list. Place all state transform functions into the state transform list. Add all
     * frame ID codes to the set of known frames.
     */
    for (FrameTransformFunction function : this.sources) {
      frameFunctions.add(function);
      knownFrames.add(function.getFromID());
      knownFrames.add(function.getToID());
      if (function instanceof StateTransformFunction) {
        stateFunctions.add((StateTransformFunction) function);
      }
    }

    this.frameEngine =
        new ChainLinkEngine<FrameID, FrameTransformFunction, FrameCodeProvider<FrameTransformFunction>>(
            frameFunctions, FRAME_PROVIDER, SEPARATOR, BROKEN_LINK);

    this.stateEngine =
        new ChainLinkEngine<FrameID, StateTransformFunction, FrameCodeProvider<StateTransformFunction>>(
            stateFunctions, STATE_PROVIDER, SEPARATOR, BROKEN_LINK);

  }

  /**
   * Simple method that consolidates the handling of exceptional cases that arise when creating
   * frame or state transform functions.
   * 
   * @param fromID the originating frame ID code
   * @param toID the destination frame ID code
   * 
   * @return either null in the event no exceptional cases have occurred or an identity state
   *         transform function connecting fromID to toID in the event they are equivalent ID codes.
   * 
   * @throws FrameSourceLinkException is generated if either fromID or toID are not currently known
   *         to the instance
   */
  private StateTransformFunction handleExceptionalCases(FrameID fromID, FrameID toID)
      throws FrameSourceLinkException {

    if (!knownFrames.contains(fromID)) {
      throw new FrameSourceLinkException(
          "Provider does not contain any frame data for frame: " + fromID.getName());
    }

    if (!knownFrames.contains(toID)) {
      throw new FrameSourceLinkException(
          "Provider does not contain any frame data for frame: " + toID.getName());
    }

    if (fromID.equals(toID)) {
      return new IdentityStateTransformFunction(fromID, toID);
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
  public FrameTransformFunction createFrameTransformFunction(FrameID fromID, FrameID toID,
      @SuppressWarnings("unused") Coverage domain) {

    FrameTransformFunction result = handleExceptionalCases(fromID, toID);

    if (result != null) {
      return result;
    }

    return new ReferenceFrameTransformFunction<FrameTransformFunction>(fromID, toID, frameEngine);

  }

  /**
   * {@inheritDoc}
   * 
   * The function created is not safe for accessing from multiple threads. Create multiple copies if
   * this is required.
   */
  @Override
  public StateTransformFunction createStateTransformFunction(FrameID fromID, FrameID toID,
      @SuppressWarnings("unused") Coverage domain) {

    StateTransformFunction result = handleExceptionalCases(fromID, toID);

    if (result != null) {
      return result;
    }

    return new ReferenceStateTransformFunction(fromID, toID, stateEngine);
  }

  @Override
  public Set<FrameID> getKnownFrames(Set<FrameID> buffer) {
    buffer.addAll(knownFrames);
    return buffer;
  }

  @Override
  public boolean isAwareOf(FrameID id) {
    return knownFrames.contains(id);
  }

  @Override
  public List<FrameTransformFunction> getFrameSourcesInLoadOrder() {
    return sources;
  }

}


/**
 * Implementation of the state transform function that simply supplies a fixed offset, identity
 * matrix for all time. It is designed to supply callers that request transforms (either state or
 * frame) that connect the equal frame ID codes.
 */
class IdentityStateTransformFunction implements StateTransformFunction {

  private final FrameID toID;
  private final FrameID fromID;

  /**
   * Construct an identity state transform that connects the two, equal ID codes.
   * 
   * @param toID a frame ID code
   * @param fromID another frame ID code, equivalent (through equals()) to toID
   */
  public IdentityStateTransformFunction(FrameID toID, FrameID fromID) {
    this.toID = toID;
    this.fromID = fromID;
  }

  @Override
  public StateTransform getStateTransform(@SuppressWarnings("unused") double time,
      StateTransform buffer) {
    buffer = buffer == null ? new StateTransform() : buffer;
    buffer.setRotation(RotationMatrixIJK.IDENTITY);
    buffer.setRotationDerivative(MatrixIJK.ZEROS);
    return buffer;
  }

  @Override
  public Coverage getCoverage() {
    return Coverage.ALL_TIME;
  }

  @Override
  public FrameID getFromID() {
    return this.fromID;
  }

  @Override
  public FrameID getToID() {
    return this.toID;
  }

  @Override
  public RotationMatrixIJK getTransform(@SuppressWarnings("unused") double time,
      RotationMatrixIJK buffer) {
    buffer = buffer == null ? new RotationMatrixIJK() : buffer;
    buffer.setTo(RotationMatrixIJK.IDENTITY);
    return buffer;
  }

}


/**
 * Implementation of the {@link CodeProvider} interface required by the chain link engine utilized
 * internally.
 * 
 * @param <F> a function that extends FrameTransformFunction
 */
class FrameCodeProvider<F extends FrameTransformFunction> implements CodeProvider<FrameID, F> {

  @Override
  public FrameID getLeafCode(F function) {
    return function.getFromID();
  }

  @Override
  public FrameID getNodeCode(F function) {
    return function.getToID();
  }

  @Override
  public boolean validAt(F function, double time) {
    return function.getCoverage().contains(time);
  }

}
