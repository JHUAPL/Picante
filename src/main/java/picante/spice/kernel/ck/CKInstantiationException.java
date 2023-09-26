package picante.spice.kernel.ck;

import picante.spice.kernel.KernelInstantiationException;

/**
 * Exception thrown whenever CK instantiation fails.
 */
public class CKInstantiationException extends KernelInstantiationException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  public CKInstantiationException() {
    super();
  }

  public CKInstantiationException(String message, Throwable cause) {
    super(message, cause);
  }

  public CKInstantiationException(String message) {
    super(message);
  }

  public CKInstantiationException(Throwable cause) {
    super(cause);
  }

}
