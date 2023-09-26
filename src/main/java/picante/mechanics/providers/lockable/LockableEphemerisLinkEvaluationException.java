package picante.mechanics.providers.lockable;

import java.util.List;
import picante.mechanics.EphemerisEvaluationException;
import picante.mechanics.EphemerisID;
import picante.mechanics.PositionVectorFunction;

/**
 * Runtime exception indicating the lockable implementation of the ephemeris provider interface was
 * unable to properly respond to a request to link two bodies together at the identified time.
 */
public class LockableEphemerisLinkEvaluationException extends EphemerisEvaluationException {

  /**
   * Default generated serial version ID.
   */
  private static final long serialVersionUID = 1L;

  /**
   * Construct a lockable implementation of the ephemeris provider interface link exception, when
   * the requested objects and/or frames are not connected at the specified time.
   * 
   * @param target the target body that the implementation is unable to link to the observer
   * @param observer the observing body that the implementation is unable to link to the target
   * @param time the time of the request at which the link connecting target and observer failed
   * @param linkBuffer the buffer containing the portions of the link tree that were able to
   *        connected
   */
  LockableEphemerisLinkEvaluationException(EphemerisID target, EphemerisID observer, double time,
      @SuppressWarnings("unused") List<? extends PositionVectorFunction> linkBuffer) {
    super("Unable to connect " + target.getName() + " to " + observer.getName()
        + " at the requested time: " + time);
  }

}
