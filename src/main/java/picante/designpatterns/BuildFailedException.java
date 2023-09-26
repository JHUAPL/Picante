package picante.designpatterns;

import picante.exceptions.PicanteRuntimeException;

/**
 * Runtime exception indicating a builder's build() method has failed.
 * <p>
 * While not an explicit requirement, users of the {@link Builder} pattern are encouraged to use
 * this runtime exception (or descendants of it) to indicate that a build has failed.
 * </p>
 * 
 */
public class BuildFailedException extends PicanteRuntimeException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  public BuildFailedException() {}

  public BuildFailedException(String message) {
    super(message);
  }

  public BuildFailedException(Throwable cause) {
    super(cause);
  }

  public BuildFailedException(String message, Throwable cause) {
    super(message, cause);
  }

}
