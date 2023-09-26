package picante.spice.kernelpool.content;

import picante.spice.kernel.KernelInstantiationException;

/**
 * This exception indicates some non-text kernel derived content failed instantiation. Examples
 * might be FURNSH meta-kernel keywords, the ephemeris ID code name mappings, etc.
 */
public class TextKernelContentInstantiationException extends KernelInstantiationException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  public TextKernelContentInstantiationException() {
    super();
  }

  public TextKernelContentInstantiationException(String message, Throwable cause) {
    super(message, cause);
  }

  public TextKernelContentInstantiationException(String message) {
    super(message);
  }

  public TextKernelContentInstantiationException(Throwable cause) {
    super(cause);
  }

}
