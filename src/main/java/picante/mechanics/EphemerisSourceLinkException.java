package picante.mechanics;

/**
 * Exception generated when an ephemeris provider is unable to link the requested target and
 * observer in the creation of either a position or state function.
 */
public class EphemerisSourceLinkException extends SourceException {

  /**
   * Default serial version ID.
   */
  private static final long serialVersionUID = 1L;

  public EphemerisSourceLinkException() {}

  public EphemerisSourceLinkException(String message) {
    super(message);
  }

  public EphemerisSourceLinkException(Throwable cause) {
    super(cause);
  }

  public EphemerisSourceLinkException(String message, Throwable cause) {
    super(message, cause);
  }

}
