package picante.spice.kernelpool.content;

import picante.exceptions.PicanteException;

/**
 * Exception used to indicate a failure in a data retrieval from the kernel pool due to some
 * condition on the retrieving method not being met.
 * <p>
 * It is a checked exception, because the purpose of the consumer is to verify if the retrieval
 * succeeded. This forces the caller to either wrap this exception--which contains the specifics of
 * the failure--in a meaningful runtime exception or deal with it locally.
 * </p>
 */
class KernelPoolValidationException extends PicanteException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  public KernelPoolValidationException() {
    super();
  }

  public KernelPoolValidationException(String message) {
    super(message);
  }

  public KernelPoolValidationException(String keyword, String reason) {
    super("Keyword: [" + keyword + "] failed validation.  Reason: " + reason);
  }

}
