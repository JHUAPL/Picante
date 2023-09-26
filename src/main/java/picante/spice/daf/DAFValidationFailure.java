package picante.spice.daf;

import picante.exceptions.PicanteRuntimeException;

/**
 * An exception that captures any sort of validation error that occurs when attempting to load or
 * instantiate a DAF.
 */
public class DAFValidationFailure extends PicanteRuntimeException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  public DAFValidationFailure() {}

  public DAFValidationFailure(String message) {
    super(message);
  }

  public DAFValidationFailure(Throwable cause) {
    super(cause);
  }

  public DAFValidationFailure(String message, Throwable cause) {
    super(message, cause);
  }

}
