package picante.spice.kernel.spk;

import picante.spice.kernel.KernelInstantiationException;

/**
 * Exception thrown whenever the creation of an SPK fails.
 */
public class SPKInstantiationException extends KernelInstantiationException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  public SPKInstantiationException() {}

  public SPKInstantiationException(String message) {
    super(message);
  }

  public SPKInstantiationException(Throwable cause) {
    super(cause);
  }

  public SPKInstantiationException(String message, Throwable cause) {
    super(message, cause);
  }

}
