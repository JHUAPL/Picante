package picante.time;

public class InvalidUTCStringException extends RuntimeException {

  private static final long serialVersionUID = 1L;

  public InvalidUTCStringException(String utcString) {
    super("Invalid UTC String: " + utcString);
  }

}
