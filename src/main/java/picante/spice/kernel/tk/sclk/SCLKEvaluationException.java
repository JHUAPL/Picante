package picante.spice.kernel.tk.sclk;

import picante.exceptions.PicanteRuntimeException;

/**
 * Runtime exception used to indicate an error in the evaluation of the desired SCLK conversion.
 */
public class SCLKEvaluationException extends PicanteRuntimeException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  public SCLKEvaluationException(int clockID) {
    super(createMessageLeader(clockID));
  }

  public SCLKEvaluationException(int clockID, String message) {
    super(createMessageLeader(clockID) + ". Problem: " + message);
  }

  public SCLKEvaluationException(int clockID, Throwable cause) {
    super(createMessageLeader(clockID), cause);
  }

  public SCLKEvaluationException(int clockID, String message, Throwable cause) {
    super(createMessageLeader(clockID) + ". Problem: " + message, cause);
  }

  private static String createMessageLeader(int clockID) {
    return "Unable to evaluate conversion for SCLK[" + clockID + "]";
  }
}
