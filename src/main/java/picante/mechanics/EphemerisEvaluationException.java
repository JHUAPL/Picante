package picante.mechanics;

/**
 * If an attempt to evaluate a position or state vector function fails for any reason, this runtime
 * exception or derivatives of it should be thrown to indicate the failure.
 * <p>
 * The reason this is a runtime exception is that requesting position or state transforms from the
 * associated functions is something that in many applications will occur frequently and in a
 * distributed sense throughout the code. As such, these methods should generate runtime exceptions
 * only, so as not to annoy developers utilizing these methods in the interface.
 * </p>
 */
public class EphemerisEvaluationException extends EvaluationRuntimeException {

  /**
   * Default serial version ID.
   */
  private static final long serialVersionUID = 1L;

  public EphemerisEvaluationException() {}

  public EphemerisEvaluationException(String message) {
    super(message);
  }

  public EphemerisEvaluationException(Throwable cause) {
    super(cause);
  }

  public EphemerisEvaluationException(String message, Throwable cause) {
    super(message, cause);
  }

}
