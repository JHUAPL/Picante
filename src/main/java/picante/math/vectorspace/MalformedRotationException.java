package picante.math.vectorspace;

/**
 * Simple exception that indicates the specification of a rotation matrix is invalid.
 */
public class MalformedRotationException extends Exception {

  /**
   * Default serialization ID.
   */
  private static final long serialVersionUID = 1L;

  public MalformedRotationException() {}

  public MalformedRotationException(String message) {
    super(message);
  }

  public MalformedRotationException(Throwable cause) {
    super(cause);
  }

  public MalformedRotationException(String message, Throwable cause) {
    super(message, cause);
  }

}
