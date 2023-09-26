package picante.spice;

import picante.designpatterns.BuildFailedException;
import picante.mechanics.EphemerisID;
import picante.mechanics.FrameID;

/**
 * Extension of the build failed exception that indicates a binding conflict has occurred.
 * Individual, static inner classes define the various types of conflicts that can occur.
 */
public class BindingConflictException extends BuildFailedException {

  /**
   * Default serial version UID.
   */
  private static final long serialVersionUID = 1L;

  private static String buildIDString(EphemerisID id) {
    return id.getName() + "[" + id + "]";
  }

  private static String buildIDString(FrameID id) {
    return id.getName() + "[" + id + "]";
  }

  private BindingConflictException(String message) {
    super(message);
  }

  private BindingConflictException(String message, Throwable cause) {
    super(message, cause);
  }

  /**
   * Indicates equivalent IDs have been bound to multiple, distinct integer codes.
   */
  public static class DuplicateEphemerisCode extends BindingConflictException {

    /**
    	 * 
    	 */
    private static final long serialVersionUID = 1L;

    public DuplicateEphemerisCode(EphemerisID existingID, int existingCode, EphemerisID newID,
        String newString) {
      super("Unable to bind ephemeris ID: " + buildIDString(newID) + " to " + newString + " as "
          + buildIDString(existingID) + " is already bound to the integer code: " + existingCode
          + " which is tied to it.");
    }

  }

  /**
   * Indicates an attempt to bind non-equivalent IDs to the same integer code.
   */
  public static class DuplicateEphemerisID extends BindingConflictException {

    /**
    	 * 
    	 */
    private static final long serialVersionUID = 1L;

    public DuplicateEphemerisID(Throwable cause) {
      super("An attempt to repurpose a built-in code has occurred, "
          + "without removing the original binding.", cause);
    }

    public DuplicateEphemerisID(EphemerisID existingID, int existingCode, EphemerisID newID,
        String newString, Throwable cause) {
      super("Unable to bind ephemeris ID: " + buildIDString(newID) + " to " + newString + " as "
          + buildIDString(existingID) + " was already bound to a different object with code: "
          + existingCode, cause);
    }

  }

  public static class DuplicateFrameCode extends BindingConflictException {

    /**
    	 * 
    	 */
    private static final long serialVersionUID = 1L;

    public DuplicateFrameCode(FrameID existingID, int existingCode, FrameID newID,
        String newString) {
      super("Unable to bind frame ID: " + buildIDString(newID) + " to " + newString + " as "
          + buildIDString(existingID) + " is already bound to the integer code: " + existingCode
          + " which is tied to it.");
    }

  }

  public static class DuplicateFrameID extends BindingConflictException {

    /**
    	 * 
    	 */
    private static final long serialVersionUID = 1L;

    public DuplicateFrameID(Throwable cause) {
      super("An attempt to repurpose a built-in code has occurred, "
          + "without removing the original binding.", cause);
    }

    public DuplicateFrameID(FrameID existingID, int existingCode, FrameID newID, String newString,
        Throwable cause) {
      super("Unable to bind frame ID: " + buildIDString(newID) + " to " + newString + " as "
          + buildIDString(existingID) + " was already bound to a different frame with code: "
          + existingCode, cause);
    }

  }

}
