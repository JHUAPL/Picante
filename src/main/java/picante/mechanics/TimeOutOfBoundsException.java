package picante.mechanics;

import picante.exceptions.PicanteRuntimeException;

/**
 * An exception that indicates a time requested from a data source is outside of the supported
 * bounds.
 */
public class TimeOutOfBoundsException extends PicanteRuntimeException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  public TimeOutOfBoundsException(double time) {
    super("The time " + time + " lies outside the bounds of the support.");
  }

  public TimeOutOfBoundsException() {}

  public TimeOutOfBoundsException(String message) {
    super(message);
  }

  public TimeOutOfBoundsException(Throwable cause) {
    super(cause);
  }

  public TimeOutOfBoundsException(String message, Throwable cause) {
    super(message, cause);
  }

}
