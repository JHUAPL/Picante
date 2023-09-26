package picante.mechanics.providers.lockable;

import java.util.List;
import java.util.ListIterator;
import com.google.common.collect.Lists;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.FrameID;
import picante.mechanics.StateTransform;
import picante.mechanics.StateTransformFunction;
import picante.mechanics.utilities.ChainLinkEngine;

/**
 * Implementation of the state transform function that is backed by the reference implementation's
 * chain link engine. Requests made to this function for state or frame transforms are evaluated at
 * the time of request, and as such may result in runtime exceptions.
 * <p>
 * Due to the way in which calculations are buffered, this implementation of the state transform
 * function interface is not safely accessible from multiple threads.
 * </p>
 * <p>
 * Note, this implementation inherits directly from the reference implementation of the frame
 * transform function. It, however, utilizes the state transform function engine supplied to this
 * constructor. So a frame transform function created from the same reference implementation may
 * produce different results than a state transform function produced from the same provider that is
 * used as a frame transform function.
 * </p>
 */
class LockableStateTransformFunction extends LockableFrameTransformFunction<StateTransformFunction>
    implements StateTransformFunction {

  /**
   * Constructs a reference state transform function from the supplied ID codes and instance of the
   * chain link engine.
   * 
   * @param fromID the frame ID of the originating frame
   * @param toID the frame ID of the destination frame
   * @param engine the chain link engine with the linking state transform functions.
   */
  LockableStateTransformFunction(FrameID fromID, FrameID toID,
      ChainLinkEngine<FrameID, StateTransformFunction> engine) {
    super(fromID, toID, engine);
  }

  /**
   * {@inheritDoc}
   * 
   * @throws LockableFrameLinkEvaluationException if the function is unable to link the to and from
   *         codes at the requested time
   */
  @Override
  public StateTransform getStateTransform(double time, StateTransform buffer) {

    buffer = buffer == null ? new StateTransform() : buffer;

    StateTransform transform = new StateTransform();

    List<StateTransformFunction> linkBuffer = Lists.newLinkedList();

    if (!engine.populateLinkage(fromID, toID, time, linkBuffer)) {
      throw new LockableFrameLinkEvaluationException(fromID, toID, time, linkBuffer);
    }

    buffer.setRotationDerivative(MatrixIJK.ZEROS);
    buffer.setRotation(RotationMatrixIJK.IDENTITY);

    ListIterator<StateTransformFunction> iterator = linkBuffer.listIterator();
    StateTransformFunction function = iterator.next();

    while (!function.equals(LockableFrameProvider.SEPARATOR)) {
      function.getStateTransform(time, transform);

      StateTransform.mxm(transform, buffer, buffer);

      if (!iterator.hasNext()) {
        return buffer;
      }

      function = iterator.next();

    }

    while (iterator.hasNext()) {
      function = iterator.next();
      function.getStateTransform(time, transform);

      StateTransform.mixm(transform, buffer, buffer);
    }

    return buffer;
  }

}
