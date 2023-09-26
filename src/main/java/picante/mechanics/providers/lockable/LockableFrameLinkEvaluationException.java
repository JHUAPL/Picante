package picante.mechanics.providers.lockable;

import java.util.List;
import picante.mechanics.FrameEvaluationException;
import picante.mechanics.FrameID;
import picante.mechanics.FrameTransformFunction;

/**
 * Runtime exception indicating the lockable implementation of the frame provider interface was
 * unable to successfully link two requested frames at the identified time.
 */
public class LockableFrameLinkEvaluationException extends FrameEvaluationException {

  /**
   * Default generated serial version ID.
   */
  private static final long serialVersionUID = 1L;

  /**
   * Construct a lockable implementation of the frame provider interface link exception, when the
   * requested frames are not connected at the specified time.
   * 
   * @param from the frame from which the implementation is unable to create a transform
   * @param to the frame to which the implementation is unable to create a transform
   * @param time the time at which the link is connecting from to to was requested
   * @param linkBuffer the buffer containing the portions of the link tree that were able to be
   *        connected.
   */
  LockableFrameLinkEvaluationException(FrameID from, FrameID to, double time,
      @SuppressWarnings("unused") List<? extends FrameTransformFunction> linkBuffer) {
    super("Unable to connect " + from.getName() + " to " + to.getName() + " at the requested time: "
        + time);
  }

  /*
   * TODO: Add additional methods to query the chains leaving from and to, with the disconnect in
   * the appropriate place.
   */

}
