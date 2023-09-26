package picante.spice.kernel.tk.fk;

import picante.spice.kernel.KernelInstantiationException;

public class FKInstantiationException extends KernelInstantiationException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  public FKInstantiationException() {}

  public FKInstantiationException(String message) {
    super(message);
  }

  public FKInstantiationException(Throwable cause) {
    super(cause);
  }

  public FKInstantiationException(String message, Throwable cause) {
    super(message, cause);
  }

}
