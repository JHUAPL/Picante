package picante.mechanics;

/**
 * An exception thrown to indicate some attempt to access the contents of a source of data in this
 * package has failed.
 */
public class SourceException extends RuntimeException {

  /**
   * Default serial version ID.
   */
  private static final long serialVersionUID = 1L;

  public SourceException() {}

  public SourceException(String message) {
    super(message);
  }

  public SourceException(Throwable cause) {
    super(cause);
  }

  public SourceException(String message, Throwable cause) {
    super(message, cause);
  }

}
