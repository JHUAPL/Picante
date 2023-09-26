package picante.exceptions;

import picante.math.vectorspace.UnwritableVectorIJ;
import picante.math.vectorspace.UnwritableVectorIJK;

/**
 * Runtime exception to be thrown when a function cannot be evaluated.
 * 
 * @author G.K.Stephens
 * 
 */
public class FunctionEvaluationException extends PicanteRuntimeException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  /**
   * Constructs a new runtime exception. The cause is not initialized, and may subsequently be
   * initialized by a call to {@link #initCause}.
   * 
   */
  public FunctionEvaluationException() {
    super(String.format("The function is not able to evaluate at the supplied value"));
  }

  /**
   * Constructs a new runtime exception. The cause is not initialized, and may subsequently be
   * initialized by a call to {@link #initCause}.
   * 
   * @param x the value that caused this exception to be generated
   */
  public FunctionEvaluationException(double x) {
    super(String.format("The function is not able to evaluate at the value %s", x));
  }

  /**
   * Constructs a new runtime exception with the specified detail message. The cause is not
   * initialized, and may subsequently be initialized by a call to {@link #initCause}.
   * 
   * @param x the value that caused this exception to be generated
   * @param message the detail message. The detail message is saved for later retrieval by the
   *        {@link #getMessage()} method
   */
  public FunctionEvaluationException(double x, String message) {
    super(String.format("The function is not able to evaluate at the value %s. %s", x, message));
  }

  /**
   * Constructs a new runtime exception with the specified detail message. The cause is not
   * initialized, and may be subsequently initialized with a call to {@link #initCause(Throwable)}.
   * 
   * @param x the value that caused this exception to be generated
   * 
   * @param message the detail message.
   * 
   */
  public FunctionEvaluationException(UnwritableVectorIJ x, String message) {
    super(String.format("The function is not able to evaluate at the value %s. %s", x, message));
  }

  /**
   * Constructs a new runtime exception with the specified detail message. The cause is not
   * initialized, and may be subsequently initialized with a call to {@link #initCause(Throwable)}.
   * 
   * @param x the value that caused this exception to be generated
   * 
   * @param message the detail message.
   * 
   */
  public FunctionEvaluationException(UnwritableVectorIJK x, String message) {
    super(String.format("The function is not able to evaluate at the value %s. %s", x, message));
  }

}
