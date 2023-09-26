package picante.exceptions;

/**
 * Parent of all runtime exceptions generated by the picante library.
 */
public class PicanteRuntimeException extends RuntimeException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  public PicanteRuntimeException() {}

  public PicanteRuntimeException(String message) {
    super(message);
  }

  public PicanteRuntimeException(Throwable cause) {
    super(cause);
  }

  public PicanteRuntimeException(String message, Throwable cause) {
    super(message, cause);
  }

}