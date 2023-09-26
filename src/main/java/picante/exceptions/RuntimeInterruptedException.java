package picante.exceptions;

/**
 * A {@link RuntimeException} intended to perform the same function as {@link InterruptedException}
 * in cases where the API can not declare the checked variant provided by the JDK.
 * 
 */
public class RuntimeInterruptedException extends PicanteRuntimeException {

  private static final long serialVersionUID = 1L;

  /**
   * Default exception with no detail message
   */
  public RuntimeInterruptedException() {
    super();
  }

  /**
   * Exception with detail message
   * 
   * @param message tbe details
   */
  public RuntimeInterruptedException(String message) {
    super(message);
  }

}
