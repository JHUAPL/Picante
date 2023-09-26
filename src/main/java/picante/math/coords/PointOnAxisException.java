package picante.math.coords;

import picante.exceptions.PicanteRuntimeException;

public class PointOnAxisException extends PicanteRuntimeException {
  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  public PointOnAxisException() {}

  public PointOnAxisException(String message) {
    super(message);
  }

  public PointOnAxisException(Throwable cause) {
    super(cause);
  }

  public PointOnAxisException(String message, Throwable cause) {
    super(message, cause);
  }
}
