package picante.exceptions;

/**
 * Runtime exception generated as a result of something failing that should never fail.
 * <p>
 * Prototypical example would be invoking the string constructor: new String(new byte[] {32, 32},
 * "ISO-8859-1") the constructor may throw an exception due to an unsupported character encoding,
 * but this should never happen in practice.
 * </p>
 * 
 */
public class BugException extends PicanteRuntimeException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  public BugException() {}

  public BugException(String message) {
    super(message);
  }

  public BugException(Throwable cause) {
    super(cause);
  }

  public BugException(String message, Throwable cause) {
    super(message, cause);
  }

}
