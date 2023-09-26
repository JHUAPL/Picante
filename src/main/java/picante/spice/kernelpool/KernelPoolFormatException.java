package picante.spice.kernelpool;

import picante.exceptions.PicanteRuntimeException;

public class KernelPoolFormatException extends PicanteRuntimeException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  public KernelPoolFormatException(String key, double value) {
    super("Unable to convert numeric value: " + value + " present in key: " + key
        + " to an integer.  It lies outside the appropriate range.");
  }

}
