package picante.mechanics;

/**
 * Exception generated when accessing the content of an ephemeris source fails for any reason.
 */
public class EphemerisSourceIOException extends SourceException {

  /**
   * Default serial version ID.
   */
  private static final long serialVersionUID = 1L;

  public EphemerisSourceIOException() {}

  public EphemerisSourceIOException(String message) {
    super(message);
  }

  public EphemerisSourceIOException(Throwable cause) {
    super(cause);
  }

  public EphemerisSourceIOException(String message, Throwable cause) {
    super(message, cause);
  }

}
