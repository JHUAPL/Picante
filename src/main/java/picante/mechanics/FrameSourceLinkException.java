package picante.mechanics;

/**
 * Exception generated when a frame provider is unable to link the requested frames in the creation
 * of a frame transform or state transform function.
 */
public class FrameSourceLinkException extends SourceException {

  /**
   * Default serial version ID.
   */
  private static final long serialVersionUID = 1L;

  public FrameSourceLinkException() {}

  public FrameSourceLinkException(String message) {
    super(message);
  }

  public FrameSourceLinkException(Throwable cause) {
    super(cause);
  }

  public FrameSourceLinkException(String message, Throwable cause) {
    super(message, cause);
  }

}
