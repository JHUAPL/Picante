package picante.mechanics;

/**
 * Exception generated when accessing the content of a frame source fails for any reason.
 */
public class FrameSourceIOException extends SourceException {

  /**
   * Default serial version ID.
   */
  private static final long serialVersionUID = 1L;

  public FrameSourceIOException() {}

  public FrameSourceIOException(String message) {
    super(message);
  }

  public FrameSourceIOException(Throwable cause) {
    super(cause);
  }

  public FrameSourceIOException(String message, Throwable cause) {
    super(message, cause);
  }

}
