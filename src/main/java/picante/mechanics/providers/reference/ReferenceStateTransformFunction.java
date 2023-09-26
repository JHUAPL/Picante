package picante.mechanics.providers.reference;

import java.util.ListIterator;
import picante.math.vectorspace.MatrixIJK;
import picante.math.vectorspace.RotationMatrixIJK;
import picante.mechanics.FrameID;
import picante.mechanics.StateTransform;
import picante.mechanics.StateTransformFunction;

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
class ReferenceStateTransformFunction extends
    ReferenceFrameTransformFunction<StateTransformFunction> implements StateTransformFunction {

  /**
   * State transform utilized as a local buffer to store intermediate results.
   */
  private final StateTransform transform = new StateTransform();

  /**
   * Constructs a reference state transform function from the supplied ID codes and instance of the
   * chain link engine.
   * 
   * @param fromID the frame ID of the originating frame
   * @param toID the frame ID of the destination frame
   * @param engine the chain link engine with the linking state transform functions.
   */
  public ReferenceStateTransformFunction(FrameID fromID, FrameID toID,
      ChainLinkEngine<FrameID, StateTransformFunction, FrameCodeProvider<StateTransformFunction>> engine) {
    super(fromID, toID, engine);
  }

  /**
   * {@inheritDoc}
   * 
   * @throws ReferenceFrameLinkEvaluationException if the function is unable to link the to and from
   *         codes at the requested time
   */
  @Override
  public StateTransform getStateTransform(double time, StateTransform buffer) {

    buffer = buffer == null ? new StateTransform() : buffer;

    if (!engine.populateLinkage(fromID, toID, time, linkBuffer)) {
      throw new ReferenceFrameLinkEvaluationException(fromID, toID, time, linkBuffer);
    }

    buffer.setRotationDerivative(MatrixIJK.ZEROS);
    buffer.setRotation(RotationMatrixIJK.IDENTITY);

    ListIterator<StateTransformFunction> iterator = linkBuffer.listIterator();
    StateTransformFunction function = iterator.next();

    while (!function.equals(ReferenceFrameProvider.SEPARATOR)) {
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
