package picante.spice.kernel.tk.pck;

import picante.spice.kernel.KernelInstantiationException;

public class TextPCKInstantiationException extends KernelInstantiationException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  public TextPCKInstantiationException() {}

  public TextPCKInstantiationException(String message) {
    super(message);
  }

  public TextPCKInstantiationException(Throwable cause) {
    super(cause);
  }

  public TextPCKInstantiationException(String message, Throwable cause) {
    super(message, cause);
  }

}
