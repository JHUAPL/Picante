package picante.spice.kernel.pck;

import picante.spice.kernel.KernelInstantiationException;

/**
 * Exception thrown whenever the creation of a binary PCK fails.
 */
public class BinaryPCKInstantiationException extends KernelInstantiationException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  public BinaryPCKInstantiationException() {
    super();
  }

  public BinaryPCKInstantiationException(String message, Throwable cause) {
    super(message, cause);
  }

  public BinaryPCKInstantiationException(String message) {
    super(message);
  }

  public BinaryPCKInstantiationException(Throwable cause) {
    super(cause);
  }

}
