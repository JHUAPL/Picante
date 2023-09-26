package picante.spice.kernel.ck;

import picante.mechanics.FrameEvaluationException;

/**
 * Exception thrown whenever CK evaluation fails.
 */
public class CKEvaluationException extends FrameEvaluationException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  public CKEvaluationException() {
    super();
  }

  public CKEvaluationException(double time) {
    super("The requested time: " + time + " lies outside the bounds of support.");
  }

  public CKEvaluationException(String message, Throwable cause) {
    super(message, cause);
  }

  public CKEvaluationException(String message) {
    super(message);
  }

  public CKEvaluationException(Throwable cause) {
    super(cause);
  }

}
