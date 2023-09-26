package picante.mechanics.providers.lockable;

import java.util.List;
import java.util.ListIterator;
import com.google.common.collect.Lists;
import picante.mechanics.Coverage;
import picante.mechanics.EphemerisEvaluationException;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;
import picante.mechanics.SourceException;
import picante.mechanics.StateTransform;
import picante.mechanics.StateTransformFunction;
import picante.mechanics.StateVector;
import picante.mechanics.StateVectorFunction;
import picante.mechanics.utilities.ChainLinkEngine;

/**
 * Implementation of the state vector function that is backed by the reference implementation's
 * chain link engine. Requests made to this function for state or position vectors are evaluated at
 * the time of the request, and as such may result in runtime exceptions.
 * <p>
 * Due to the way in which calculations are buffered, this implementation of the state vector
 * function interface is not safely accessible from multiple threads.
 * </p>
 * <p>
 * Note, this implementation inherits directly from the reference implementation of the position
 * vector function. It, however, utilizes the state vector function engine supplied to this
 * constructor. So a position vector function created from the same reference implementation may
 * produce different results than a state vector function produced from the same put that is used.
 * </p>
 */
class LockableStateVectorFunction extends LockablePositionVectorFunction<StateVectorFunction>
    implements StateVectorFunction {

  /**
   * Constructs a reference state vector function from the supplied ID codes and instance of the
   * chain link engine.
   * 
   * @param targetID the object's ephemeris ID at the head of the state vector
   * @param observerID the object's ephemeris ID at the tail of the state vector
   * @param frameID the frame ID of the frame used to express vectors evaluated from this function
   * @param engine the chain link engine with the linking state vector functions
   * @param provider the instance of the reference frame provider implementation to support frame
   *        transforms
   */
  LockableStateVectorFunction(EphemerisID targetID, EphemerisID observerID, FrameID frameID,
      ChainLinkEngine<EphemerisID, StateVectorFunction> engine, LockableFrameProvider provider) {
    super(targetID, observerID, frameID, engine, provider);
  }

  /**
   * {@inheritDoc}
   * 
   * @throws LockableEphemerisLinkEvaluationException if the function is unable to link the target
   *         and observer codes at the requested time.
   * 
   * @throws LockableFrameLinkEvaluationException if the function is unable to apply the necessary
   *         frame transformations to derive the desired state at the requested time
   */
  @Override
  public StateVector getState(double time, StateVector buffer) {

    buffer = buffer == null ? new StateVector() : buffer;

    List<StateVectorFunction> linkBuffer = Lists.newLinkedList();

    /*
     * First see if the engine is capable of connecting the target and observer IDs at the requested
     * time.
     */
    if (!engine.populateLinkage(targetID, observerID, time, linkBuffer)) {
      throw new LockableEphemerisLinkEvaluationException(targetID, observerID, time, linkBuffer);
    }

    /*
     * Now, at this point we are going to compute the state of the target relative to the common
     * node (indicated by the presence of a SEPARATOR in the linkBuffer) in the frame defining the
     * last link prior to the common node. Then, we will compute the state of the observer relative
     * to the common node in the frame defining the last link in the reverse chain.
     */
    ListIterator<StateVectorFunction> iterator = linkBuffer.listIterator();
    StateVectorFunction function = iterator.next();

    FrameID fwdFrame;
    FrameID bwdFrame;
    FrameID currFrame;

    StateVector fwdBuffer = new StateVector();
    StateVector bwdBuffer = new StateVector();
    StateVector tmpBuffer = new StateVector();
    StateTransform xfmBuffer = new StateTransform();

    if (!function.equals(LockableEphemerisProvider.SEPARATOR)) {

      /*
       * Since the first link in the chain isn't the separator, we have to evaluate the forward
       * chain.
       */
      fwdFrame = function.getFrameID();
      function.getState(time, fwdBuffer);

      /*
       * Handle the simple case, when the chain has but one link connecting the target to the
       * observer.
       */
      if (!iterator.hasNext()) {
        transformStateInPlace(fwdFrame, frameID, time, fwdBuffer, xfmBuffer);
        buffer.setTo(fwdBuffer);
        return buffer;
      }

      function = iterator.next();

      /*
       * And now, since we have more than one link in the chain, loop through until we reach the
       * separator or the end of the chain.
       */
      while (!function.equals(LockableEphemerisProvider.SEPARATOR)) {

        function.getState(time, tmpBuffer);

        /*
         * Pull up the currently buffered fwdBuffer to the same frame.
         */
        currFrame = function.getFrameID();
        transformStateInPlace(fwdFrame, currFrame, time, fwdBuffer, xfmBuffer);

        StateVector.add(fwdBuffer, tmpBuffer, fwdBuffer);

        /*
         * Check to see if the iterator has additional links in the chain.
         */
        if (!iterator.hasNext()) {
          transformStateInPlace(currFrame, frameID, time, fwdBuffer, xfmBuffer);
          buffer.setTo(fwdBuffer);
          return buffer;
        }

        /*
         * Prepare to process the next function in the list.
         */
        fwdFrame = currFrame;
        function = iterator.next();

      }
    } else {
      fwdBuffer.clear();
      function = iterator.next();
      fwdFrame = function.getFrameID();
    }

    /*
     * At this point we have properly populated the fwdBuffer with the state connecting the common
     * node to the target. Further, the frame in which this vector is currently expressed is
     * captured in fwdFrame. Start at the end of the list and work backwards until we reach the
     * separator. Performing the same set of computations for the observer linkage.
     */
    iterator = linkBuffer.listIterator(linkBuffer.size());

    function = iterator.previous();

    bwdFrame = function.getFrameID();
    function.getState(time, bwdBuffer);

    function = iterator.previous();

    while (!function.equals(LockableEphemerisProvider.SEPARATOR)) {
      function.getState(time, tmpBuffer);

      /*
       * Pull up the current bwdBuffer to the current function's frame.
       */
      currFrame = function.getFrameID();
      transformStateInPlace(bwdFrame, currFrame, time, bwdBuffer, xfmBuffer);

      StateVector.add(bwdBuffer, tmpBuffer, bwdBuffer);

      /*
       * Prepare to process the next function in the list.
       */
      bwdFrame = currFrame;
      function = iterator.previous();
    }

    /*
     * At this point we have two states, one connecting the target to a common node (which may be
     * the zero vector) and another connecting the observer to the common node. Combine these
     * vectors into buffer and then rotate into the requested frame.
     */
    transformStateInPlace(fwdFrame, bwdFrame, time, fwdBuffer, xfmBuffer);
    StateVector.subtract(fwdBuffer, bwdBuffer, buffer);
    transformStateInPlace(bwdFrame, frameID, time, buffer, xfmBuffer);

    return buffer;
  }

  /**
   * Implementation detail method, that transforms a state vector from one frame to another in
   * place.
   * 
   * @param fromID the frame ID of the frame in which state is expressed
   * @param toID the frame ID of the frame in which state is desired
   * @param time the time at which state was computed and the state transformation is to be
   *        evaluated
   * @param state the state to move from <code>fromID</code> to <code>toID</code> in place
   */
  private void transformStateInPlace(FrameID fromID, FrameID toID, double time, StateVector state,
      StateTransform xform) {

    /*
     * Check to see if anything should be done.
     */
    if (fromID.equals(toID)) {
      return;
    }

    /*
     * Compute the transformation.
     */
    getTransform(fromID, toID).getStateTransform(time, xform).mxv(state, state);
  }

  /**
   * Overrides the transform function retrieval method to only allow state transforms available from
   * the provider to be returned.
   */
  @Override
  StateTransformFunction getTransform(FrameID fromID, FrameID toID) {
    try {
      return provider.createStateTransformFunction(fromID, toID, Coverage.ALL_TIME);
    } catch (SourceException e) {
      throw new EphemerisEvaluationException("Unable to derive state transformation.", e);
    }
  }

}
