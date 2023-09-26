package picante.spice.kernel.tk.sclk;

import picante.spice.kernel.KernelInstantiationException;

/**
 * Exception designed to indicate the failure of instantiating an SCLK.
 */
public class SCLKInstantiationException extends KernelInstantiationException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  public SCLKInstantiationException(int clockID) {
    super(createMessageLeader(clockID));
  }

  public SCLKInstantiationException(int clockID, String message) {
    super(createMessageLeader(clockID) + ". Problem: " + message);
  }

  public SCLKInstantiationException(int clockID, Throwable cause) {
    super(createMessageLeader(clockID), cause);
  }

  public SCLKInstantiationException(int clockID, String message, Throwable cause) {
    super(createMessageLeader(clockID) + ". Problem: " + message, cause);
  }

  private static String createMessageLeader(int clockID) {
    return "Unable to instantiate SCLK[" + clockID + "]";
  }

}
