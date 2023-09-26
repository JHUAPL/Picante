package picante.spice.daf;

import picante.exceptions.PicanteRuntimeException;

/**
 * Simple runtime exception that DAF interface implementors may utilize directly or subclass, and
 * use to indicate to consumers of their implementation that access to DAF content has failed for
 * some reason.
 */
public class DAFAccessException extends PicanteRuntimeException {

  /**
   * Default serial version uid.
   */
  private static final long serialVersionUID = 1L;

  public DAFAccessException() {}

  public DAFAccessException(String message) {
    super(message);
  }

  public DAFAccessException(Throwable cause) {
    super(cause);
  }

  public DAFAccessException(String message, Throwable cause) {
    super(message, cause);
  }

}
