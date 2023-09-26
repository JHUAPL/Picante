package picante.mechanics.providers.lockable;

import java.util.List;
import java.util.Set;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;
import picante.exceptions.BugException;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.Coverage;
import picante.mechanics.FrameID;
import picante.mechanics.FrameProvider;
import picante.mechanics.FrameSourceLinkException;
import picante.mechanics.FrameTransformFunction;
import picante.mechanics.StateTransform;
import picante.mechanics.StateTransformFunction;
import picante.mechanics.utilities.ChainLinkEngine;
import picante.mechanics.utilities.ChainLinkEngine.CodeProvider;

/**
 * Provides a lockable implementation of the frame provider interface. This interface creates frame
 * and state transform functions in a thread safe, but slightly inefficient manner. The coverage
 * intervals supplied to create the transform functions are ignored by the implementation. Error
 * generation is deferred to the evaluation of the function.
 * <p>
 * With regards to thread safety, instances of this provider may be used to create functions from
 * multiple threads safely. The individual functions created are thread safe.
 * </p>
 */
public class LockableFrameProvider implements FrameProvider {

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

      private BugException createException() {
        return new BugException("Implementation error.  This method should never be invoked.");
      }

      @Override
      public StateTransform getStateTransform(@SuppressWarnings("unused") double time,
          @SuppressWarnings("unused") StateTransform buffer) {
        throw createException();
      }

      @Override
      public Coverage getCoverage() {
        throw createException();
      }

      @Override
      public FrameID getFromID() {
        throw createException();
      }

      @Override
      public FrameID getToID() {
        throw createException();
      }

      @Override
      public RotationMatrixIJK getTransform(@SuppressWarnings("unused") double time,
          @SuppressWarnings("unused") RotationMatrixIJK buffer) {
        throw createException();
      }

    };
  }

  /**
   * An instance of the {@link CodeProvider} interface suitable for usage in
   * <code>ChainLinkEngine</code>s that link <code>FrameTransformFunctions</code> together.
   */
  private final static FrameCodeProvider CODE_PROVIDER = new FrameCodeProvider();

  /**
   * Marker instance used to indicate the separation between the forward evaluation of a linked
   * chain and the backward evaluation. Functions that occur prior to this in a chained list of
   * functions can be chained directly by the transform returned. Functions after, must be inverted
   * prior to their combination.
   */
  final static StateTransformFunction SEPARATOR = createMarkerFunction();

  /**
   * Marker instance used to indicate the separation between the extent of the forward chain and the
   * extent of the backward chain in the event that a connection is unable to be located. Primarily
   * intended to create more useful error messages in the event of a broken link.
   */
  final static StateTransformFunction BROKEN_LINK = createMarkerFunction();

  /**
   * Instance of the chain link engine used to connect any frame or state transform functions
   * provided to the constructor of this class.
   */
  private final ChainLinkEngine<FrameID, FrameTransformFunction> frameEngine;

  /**
   * Instance of the chain link engine used to connect any state transform functions provided to the
   * constructor of this class.
   */
  private final ChainLinkEngine<FrameID, StateTransformFunction> stateEngine;

  /**
   * The list of sources loaded into the application from lowest priority to highest priority.
   */
  private final ImmutableList<FrameTransformFunction> sources;

  /**
   * The set of frame ID codes known to the instance.
   */
  private final Set<FrameID> knownFrames;

  /**
   * Constructs an instance of the lockable frame provider from the supplied list of sources with a
   * lock type of {@link LockType#FUNCTION}.
   * <p>
   * This is the most commonly, and performant, lock type to utilize. It does require, however; that
   * all functions within a source supplied to the constructor are thread independent.
   * </p>
   * 
   * @param sources a list of sources, in order from lowest priority (low index) to highest priority
   *        (high index).
   * 
   * 
   */
  public LockableFrameProvider(List<? extends FrameTransformFunction> sources) {
    this(sources, LockType.FUNCTION);
  }

  /**
   * Constructs an instance of the lockable frame provider from the supplied list of sources.
   * 
   * @param sources a list of sources, in order from lowest priority (low index) to highest priority
   *        (high index).
   * @param lockType the type of the lock to utilize for synchronization
   * 
   * 
   */


  public LockableFrameProvider(List<? extends FrameTransformFunction> sources,
      LockSupplier lockSupplier) {


    /*
     * Capture the list of sources in load priority order.
     */
    this.sources = ImmutableList.copyOf(sources);

    ImmutableSet.Builder<FrameID> knownFramesBuilder = ImmutableSet.builder();

    /*
     * Create separate lists to receive frame transform and state transform functions.
     */
    List<FrameTransformFunction> frameFunctions = Lists.newLinkedList();
    List<StateTransformFunction> stateFunctions = Lists.newLinkedList();

    /*
     * Loop over all supplied sources. Place all state and frame transform functions into the frame
     * transform list. Place all state transform functions into the state transform list. Add all
     * frame ID codes to the set of known frames.
     */
    for (FrameTransformFunction function : this.sources) {
      frameFunctions.add(new LockingDelegateFrameTransformFunction(
          lockSupplier.getLock(this, function), function));
      knownFramesBuilder.add(function.getFromID());
      knownFramesBuilder.add(function.getToID());
      if (function instanceof StateTransformFunction) {
        stateFunctions.add(new LockingDelegateStateTransformFunction(
            lockSupplier.getLock(this, function), (StateTransformFunction) function));
      }
    }


    this.knownFrames = knownFramesBuilder.build();

    this.frameEngine = new ChainLinkEngine<>(frameFunctions, CODE_PROVIDER, SEPARATOR, BROKEN_LINK);

    this.stateEngine = new ChainLinkEngine<>(stateFunctions, CODE_PROVIDER, SEPARATOR, BROKEN_LINK);

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

  @Override
  public FrameTransformFunction createFrameTransformFunction(FrameID fromID, FrameID toID,
      @SuppressWarnings("unused") Coverage domain) {

    FrameTransformFunction result = handleExceptionalCases(fromID, toID);

    if (result != null) {
      return result;
    }

    return new LockableFrameTransformFunction<FrameTransformFunction>(fromID, toID, frameEngine);

  }

  @Override
  public StateTransformFunction createStateTransformFunction(FrameID fromID, FrameID toID,
      @SuppressWarnings("unused") Coverage domain) {

    StateTransformFunction result = handleExceptionalCases(fromID, toID);

    if (result != null) {
      return result;
    }

    return new LockableStateTransformFunction(fromID, toID, stateEngine);
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
class FrameCodeProvider implements CodeProvider<FrameID, FrameTransformFunction> {

  @Override
  public FrameID getLeafCode(FrameTransformFunction function) {
    return function.getFromID();
  }

  @Override
  public FrameID getNodeCode(FrameTransformFunction function) {
    return function.getToID();
  }

  @Override
  public boolean validAt(FrameTransformFunction function, double time) {
    return function.getCoverage().contains(time);
  }

}
