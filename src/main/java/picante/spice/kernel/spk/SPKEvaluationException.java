package picante.spice.kernel.spk;

import picante.mechanics.EphemerisEvaluationException;

/**
 * Exception thrown whenever the evaluation of an SPK segment fails.
 */
public class SPKEvaluationException extends EphemerisEvaluationException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  public SPKEvaluationException() {}

  public SPKEvaluationException(String message) {
    super(message);
  }

  public SPKEvaluationException(Throwable cause) {
    super(cause);
  }

  public SPKEvaluationException(String message, Throwable cause) {
    super(message, cause);
  }

}
