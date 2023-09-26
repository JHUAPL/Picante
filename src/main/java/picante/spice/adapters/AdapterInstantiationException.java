package picante.spice.adapters;

import picante.exceptions.PicanteException;

/**
 * Exception designed to indicate the failure of a adapter in the SPICE package to be instantiated
 * for any reason.
 */
public class AdapterInstantiationException extends PicanteException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  public AdapterInstantiationException() {
    super();
  }

  public AdapterInstantiationException(String message, Throwable cause) {
    super(message, cause);
  }

  public AdapterInstantiationException(String message) {
    super(message);
  }

  public AdapterInstantiationException(Throwable cause) {
    super(cause);
  }

}
