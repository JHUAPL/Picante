package picante.mechanics;

import picante.exceptions.PicanteRuntimeException;

/**
 * An exception indicating an evaluation of an ephemeris or frame function has failed for any
 * reason.
 */
public class EvaluationRuntimeException extends PicanteRuntimeException {

  /**
   * Default serial version ID.
   */
  private static final long serialVersionUID = 1L;

  public EvaluationRuntimeException() {}

  public EvaluationRuntimeException(String message) {
    super(message);
  }

  public EvaluationRuntimeException(Throwable cause) {
    super(cause);
  }

  public EvaluationRuntimeException(String message, Throwable cause) {
    super(message, cause);
  }

}
