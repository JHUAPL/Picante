package picante.spice.kernel;

import picante.exceptions.PicanteException;

/**
 * Exception indicating the creation of a kernel object has failed.
 * <p>
 * In general the various implementations of kernel types will subclass this exception for their own
 * purposes.
 * </p>
 */
public class KernelInstantiationException extends PicanteException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  public KernelInstantiationException() {}

  public KernelInstantiationException(String message) {
    super(message);
  }

  public KernelInstantiationException(Throwable cause) {
    super(cause);
  }

  public KernelInstantiationException(String message, Throwable cause) {
    super(message, cause);
  }

}
