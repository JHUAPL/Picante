package picante.surfaces;

import picante.exceptions.PicanteRuntimeException;

/**
 * Exception indicating that an intersection computation has failed.
 * <p>
 * In general methods that compute surface intersections, have companion methods that return a
 * boolean entitled intersects which test for intersection.
 * </p>
 */
public class NoIntersectionException extends PicanteRuntimeException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  public NoIntersectionException() {
    super();
  }

  public NoIntersectionException(String message, Throwable cause) {
    super(message, cause);
  }

  public NoIntersectionException(String message) {
    super(message);
  }

  public NoIntersectionException(Throwable cause) {
    super(cause);
  }

}
